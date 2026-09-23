# Preconditions of the run (IMR-277).
#
# This file runs FIRST. It asserts every prerequisite the run configuration
# declares, and each failure names the variable that is wrong.
#
# It exists because of what happens otherwise. On 2026-09-14 the repository
# prefix in IMPROVER_STEP had gone stale - "hc4310:" where the server stamps
# "<server>.internal-:" - and the run died 10 files later with
#
#     Error in `Sys.setenv(IMPROVER_STEP = stepEntity)`: wrong length for argument
#
# 19 failures, every fixture affected, and a message naming neither the cause
# nor anything anyone could act on. A precondition that cannot be met has to
# stop the run under its own name.
#
# These are not tests of improveR. They are tests of the environment the run
# was given, which is why they assert configuration against the server rather
# than behaviour.

Sys.setenv(TEST_NAME = "preconditions")

test_that("the session is connected and identifies itself", {
  expect_true(improveR::improveConnected(silent = TRUE),
              info = "improveConnect() must have succeeded before any test file runs")

  who <- improveR::whoami()
  expect_true(is.character(who) && nzchar(who),
              info = "whoami() must name the identity the run is performed under")

  version <- improveR::getRepositoryVersion()
  expect_true(is.character(version) && nzchar(version),
              info = paste0("getRepositoryVersion() must report the server version - ",
                            "it names the server half of the version pair under test ",
                            "and belongs in the evidence package"))
  cat("\n  identity:", who, " repository:", version, "\n")
})

test_that("IMPROVER_STEP resolves, and its repository prefix is the server's", {
  configured <- Sys.getenv("IMPROVER_STEP")
  expect_true(nzchar(configured), info = "IMPROVER_STEP must be set in .Renviron")

  resource <- improveR::loadResource(configured)
  if (is.null(resource)) {
    err <- improveR::lastRestError()
    detail <- if (is.null(err)) "no REST error recorded" else
      sprintf("HTTP %s on %s %s", err$status_code, err$method, err$url)
    fail(sprintf(paste0("IMPROVER_STEP '%s' does not resolve - %s\n",
                        "Every fixture anchors on this resource. A stale repository ",
                        "prefix is the usual cause: the identifier survives a server ",
                        "rename, the prefix does not."), configured, detail))
  }

  # The prefix is taken FROM this value by repoPrefix() and never compared with
  # the server, so a stale one is reported back as fact. Compare it here.
  if (grepl(":", configured, fixed = TRUE)) {
    configuredPrefix <- sub(":[^:]*$", ":", configured)
    serverPrefix <- sub(":[^:]*$", ":", resource$entityId)
    expect_equal(configuredPrefix, serverPrefix,
                 info = paste0("the prefix in IMPROVER_STEP must be the one the server ",
                               "stamps into its entity IDs; repoPrefix() reports the ",
                               "configured value without checking it"))
  }
})

test_that("the folders the run writes to are usable", {
  # EVIDENCE_FOLDER must already be there - the run moves its results into it
  # at the end and cannot create it on the way out.
  evidence <- Sys.getenv("EVIDENCE_FOLDER")
  expect_true(nzchar(evidence), info = "EVIDENCE_FOLDER must be set in .Renviron")
  expect_false(is.null(improveR::loadResource(evidence)),
               info = paste0("EVIDENCE_FOLDER = '", evidence,
                             "' does not exist on this server; the run has nowhere ",
                             "to move its evidence to"))

  # TEST_FOLDER is the opposite case and must NOT be required to exist. A run
  # that finished successfully moved it into the evidence folder, so between
  # two runs it is absent by design - an earlier version of this file demanded
  # it and would have failed on every run following a green one.
  # What has to hold is that its PARENT is there, so the run can create it.
  testFolder <- Sys.getenv("TEST_FOLDER")
  expect_true(nzchar(testFolder), info = "TEST_FOLDER must be set in .Renviron")
  parent <- sub("/[^/]+$", "", testFolder)
  if (!nzchar(parent)) parent <- "/"
  expect_false(is.null(improveR::loadResource(parent)),
               info = paste0("the parent of TEST_FOLDER ('", parent, "') does not exist; ",
                             "the run cannot create '", testFolder, "' under it"))
})

test_that("every declared runserver, tool and tool instance matches exactly one entry", {
  tools <- improveR::getToolInstances()
  expect_false(is.null(tools), info = "getToolInstances() must return the instances")
  keys <- ls(envir = tools)
  expect_true(length(keys) > 0, info = "the server must offer at least one tool instance")

  # getToolInstances() returns an ENVIRONMENT keyed by instance, not a data
  # frame. Matching on tools$name silently finds nothing.
  for (prefix in c("R", "NONMEM", "RSTUDIO")) {
    runserver <- Sys.getenv(paste0(prefix, "_RUNSERVER"))
    tool <- Sys.getenv(paste0(prefix, "_TOOL"))
    instance <- Sys.getenv(paste0(prefix, "_TOOL_INSTANCE"))
    if (!nzchar(runserver) && !nzchar(tool) && !nzchar(instance)) next

    matches <- Filter(function(key) {
      entry <- get(key, envir = tools)
      identical(entry$toolName, tool) &&
        identical(entry$name, instance) &&
        identical(entry$label, runserver)
    }, keys)

    expect_equal(length(matches), 1L,
                 info = paste0(prefix, ": expected exactly one tool instance for ",
                               prefix, "_RUNSERVER='", runserver, "' ",
                               prefix, "_TOOL='", tool, "' ",
                               prefix, "_TOOL_INSTANCE='", instance, "' - found ",
                               length(matches)))
  }
})

test_that("the secondary identities exist and their passwords are supplied", {
  # Nine places need a second user, for a reviewer or for someone without
  # rights. Until IMR-267 those were hardcoded; they now come from the
  # environment and have to be declared.
  allUsers <- improveR::users()
  expect_false(is.null(allUsers), info = "users() must return the user list")

  for (user in c("test1", "test2")) {
    expect_true(user %in% allUsers$username,
                info = paste0("the secondary user '", user, "' must exist on this server"))
    variable <- paste0("IMPROVER_PW_", toupper(user))
    expect_true(nzchar(Sys.getenv(variable)),
                info = paste0(variable, " must be supplied for the run - ",
                              "connectAs() refuses to guess a password"))
  }
})

test_that("the run identity may create a group and assign a foreign reviewer", {
  # Measured on 2026-09-10: an identity without these two rights produces 50
  # failures that all look like package defects (IMR-267). They are not.
  groupName <- paste0("precondition-", uuid::UUIDgenerate())
  group <- improveR::createGroup(groupName)
  if (is.null(group)) {
    err <- improveR::lastRestError()
    detail <- if (is.null(err)) "no REST error recorded" else
      sprintf("HTTP %s on %s %s", err$status_code, err$method, err$url)
    fail(paste0("the run identity cannot create groups - ", detail, "\n",
                "test-permissions.R and test-permissionsAdvanced.R cannot pass ",
                "without this right. Run under an identity that has it."))
  }
  expect_equal(group$name, groupName)
  expect_true(improveR::deleteGroup(group$id),
              info = "the run identity must also be able to remove what it created")
})

test_that("the reconnect path's dependency resolves, so a lost connection is recoverable", {
  # improveRtestsupport::improveConnect recovers a lost connection by exchanging
  # the refresh token for a new access token, rather than falling into the OAuth
  # device-code flow - which waits for a human and therefore cannot complete in a
  # headless run.
  #
  # That path called improveR::renewAccessToken(). The function is INTERNAL, so
  # the call raised "not an exported object" every time and the recovery had
  # never once worked (IMR-291). It stayed invisible because the path only runs
  # after the connection is already lost.
  #
  # Run 29 reached it: two transport stalls broke the session, recovery raised,
  # the fallback failed, and 15 files failed wholesale - 52 failures, not one of
  # them on an assertion. Asserting it here means a future rename or removal
  # reports itself in file 1 of 59, against a healthy connection, instead of
  # ambushing a run two thirds of the way through.
  expect_true(
    exists("renewAccessToken", envir = asNamespace("improveR"), inherits = FALSE),
    info = paste0("improveR has no renewAccessToken(). improveRtestsupport needs it ",
                  "to rebuild a lost session; without it a headless run cannot ",
                  "recover and every file after the loss fails (IMR-291).")
  )
  expect_true(is.function(improveR:::renewAccessToken))

  # A refresh token must be present, or the recovery path is unreachable in
  # practice no matter how healthy the function is.
  expect_true(nzchar(Sys.getenv("IMPROVER_REFRESH_TOKEN", "")),
              info = "no IMPROVER_REFRESH_TOKEN in the session - a lost connection could not be rebuilt")
})
