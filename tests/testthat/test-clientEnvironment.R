# Client environment and token registry — no server, no network (IMR-281).
#
# Five exported functions that read the session's own state. All five were
# among those the suite never entered (run 25, 2026-09-14) and all five can be
# checked without infrastructure, which is the point: 3.3 % of the suite runs
# without a server today, and REQ-REPLAY-001 needs that share to grow.
#
# Each block restores what it changed. These functions read process-wide
# environment variables, and the runner's fixed file order makes any leftover
# visible to every file after this one (IMR-265, and F41 for what that costs).

Sys.setenv(TEST_NAME = "clientEnvironment")

# --- getLogFile: three states, not two --------------------------------------

test_that("getLogFile reports NULL while logging goes to the console|ics2279", {
  original <- Sys.getenv("improver.logfile", unset = NA)
  on.exit({
    if (is.na(original)) Sys.unsetenv("improver.logfile")
    else Sys.setenv(improver.logfile = original)
  }, add = TRUE)

  Sys.unsetenv("improver.logfile")
  expect_null(improveR::getLogFile(),
              info = "with no log file configured the answer is NULL")

  # NULL means "console", not "logging is off". That distinction is the whole
  # contract and a test that only checks the happy path misses it.
  Sys.setenv(improver.logfile = "")
  expect_null(improveR::getLogFile(),
              info = "an empty value must read as NULL, not as a path named ''")
})

test_that("getLogFile returns the configured path, and stops doing so once it is unset|ics2279", {
  original <- Sys.getenv("improver.logfile", unset = NA)
  on.exit({
    if (is.na(original)) Sys.unsetenv("improver.logfile")
    else Sys.setenv(improver.logfile = original)
  }, add = TRUE)

  path <- file.path(tempdir(), paste0("imr281-", as.integer(Sys.time()), ".log"))
  Sys.setenv(improver.logfile = path)
  expect_equal(improveR::getLogFile(), path)

  Sys.unsetenv("improver.logfile")
  expect_null(improveR::getLogFile(),
              info = "the third state: configured, then unset, must return to NULL")
})

# --- the two directories, and that they are two ------------------------------

test_that("the workspace and internal directories are usable and distinct|ics2279", {
  workspace <- improveR::getImproveWorkspaceDir()
  internal  <- improveR::getImproveInternalDir()

  for (d in list(c("workspace", workspace), c("internal", internal))) {
    expect_true(is.character(d[2]) && nzchar(d[2]),
                info = paste("the", d[1], "directory must be a non-empty path"))
    expect_true(dir.exists(d[2]),
                info = paste("the", d[1], "directory must exist after being asked for:", d[2]))
  }

  # Their distinction is their reason to exist: what is written to the
  # workspace belongs to the user, what is written to the internal directory
  # belongs to improve. One path for both would silently mix them.
  expect_false(identical(normalizePath(workspace), normalizePath(internal)),
               info = paste0("workspace and internal directory must differ; both are '",
                             workspace, "'"))
})

test_that("IMPROVER_WORKSPACE overrides the default and is created if absent|ics2279", {
  original <- Sys.getenv("IMPROVER_WORKSPACE", unset = NA)
  on.exit({
    if (is.na(original)) Sys.unsetenv("IMPROVER_WORKSPACE")
    else Sys.setenv(IMPROVER_WORKSPACE = original)
  }, add = TRUE)

  wanted <- file.path(tempdir(), paste0("imr281-ws-", as.integer(Sys.time())))
  expect_false(dir.exists(wanted), info = "the fixture directory must not exist yet")

  Sys.setenv(IMPROVER_WORKSPACE = wanted)
  got <- improveR::getImproveWorkspaceDir()
  expect_equal(normalizePath(got), normalizePath(wanted),
               info = "the configured workspace must win over the platform default")
  expect_true(dir.exists(wanted),
              info = "a configured directory that does not exist yet must be created")

  unlink(wanted, recursive = TRUE)
})

# --- the token registry and the token itself ---------------------------------

test_that("listTokenRefreshers reflects what is registered, without duplicating|ics2071", {
  before <- improveR::listTokenRefreshers()
  expect_true(is.null(before) || is.character(before),
              info = "the registry must answer with names or with nothing")

  improveRtestsupport::registerFileTokenRefresher()
  after <- improveR::listTokenRefreshers()
  expect_true("file" %in% after,
              info = "a refresher that was just registered must be listed")

  # Registering the same name twice must not make it appear twice - a caller
  # iterating the list would otherwise refresh through it more than once.
  improveRtestsupport::registerFileTokenRefresher()
  again <- improveR::listTokenRefreshers()
  expect_equal(sum(again == "file"), 1L,
               info = paste0("'file' must appear exactly once; got: ",
                             paste(again, collapse = ", ")))
})

test_that("getCurrentTokenData reports the token in the environment, or NULL|ics2071", {
  original <- Sys.getenv("IMPROVER_TOKEN", unset = NA)
  on.exit({
    if (is.na(original)) Sys.unsetenv("IMPROVER_TOKEN")
    else Sys.setenv(IMPROVER_TOKEN = original)
  }, add = TRUE)

  # No token: NULL, not an empty list. A caller checking is.null() would
  # otherwise proceed with a list of empty strings.
  Sys.unsetenv("IMPROVER_TOKEN")
  expect_null(improveR::getCurrentTokenData(),
              info = "without a token the answer is NULL")

  marker <- paste0("imr281-token-", as.integer(Sys.time()))
  Sys.setenv(IMPROVER_TOKEN = marker)
  data <- improveR::getCurrentTokenData()
  expect_false(is.null(data))
  expect_equal(data$IMPROVER_TOKEN, marker,
               info = "the token reported must be the one in the environment, not a cached copy")

  # The other fields the contract names must be present even when empty, so a
  # caller can read them without checking each for existence first.
  for (field in c("IMPROVER_TOKEN_EXPIRATION", "IMPROVER_REFRESH_TOKEN",
                  "IMPROVER_LAST_ACCESS", "IMPROVER_REPO_URL", "IMPROVER_USER")) {
    expect_true(field %in% names(data),
                info = paste0("getCurrentTokenData must carry ", field,
                              "; it has: ", paste(names(data), collapse = ", ")))
  }
})
