# checkConnect and improveInit (ics2050, IMR-290)
#
# The last two of the thirteen exported functions the suite never entered
# (C9 section 7). Both change session state, which is why they are in one file
# of their own and why each block restores what it found - the runner's file
# order makes anything left behind visible to every file that follows (IMR-265).
#
# checkConnect: measured on 2026-09-15, every path returns invisible(TRUE).
# Connected, disconnected, no connection at all - all TRUE. A caller cannot
# learn from the return value whether the connection was fine or had to be
# rebuilt, and cannot learn that anything failed. The assertions below pin that
# as it is; whether it SHOULD be able to answer FALSE is IMR-290's open
# question. If that is ever decided, the block named "always TRUE" fails, which
# is the point of writing it down.
#
# improveInit: works. It sources a module file from the repository and calls the
# <basename>.init function inside it with the module's folder path. The test
# asserts what the init function was HANDED and that it actually ran - an
# is.environment() check would pass on an environment that no init ever touched.

Sys.setenv(TEST_NAME = "sessionLifecycle")

# Restores the session the runner set up: the test-support connect, which drives
# the headless OAuth, plus editable. Used after every block that disturbs it.
restoreSession <- function() {
  improveRtestsupport::improveConnect()
  improveR::setEditable(TRUE)
}

test_that("checkConnect returns TRUE and leaves a live session alone|ics2050", {
  restoreSession()
  on.exit(restoreSession(), add = TRUE)

  expect_true(improveR::improveConnected(silent = TRUE))
  result <- improveR::checkConnect()

  expect_true(is.logical(result))
  expect_length(result, 1L)
  expect_true(result)
  expect_true(improveR::improveConnected(silent = TRUE),
              info = "checking a healthy connection must not break it")

  # The step still resolves afterwards - the check must not have disturbed the
  # configuration it verified.
  expect_false(is.null(improveR::loadResource(Sys.getenv("IMPROVER_STEP"))))
})

test_that("checkConnect rebuilds a session that was disconnected|ics2050", {
  restoreSession()
  on.exit(restoreSession(), add = TRUE)

  improveR::improveDisconnect()
  expect_false(improveR::improveConnected(silent = TRUE),
               info = "the fixture must actually be disconnected, or this proves nothing")

  result <- improveR::checkConnect()

  expect_true(result)
  expect_true(improveR::improveConnected(silent = TRUE),
              info = "checkConnect must leave a usable session behind")
  # Usable, not merely flagged as connected.
  expect_false(is.null(improveR::loadResource(Sys.getenv("IMPROVER_STEP"))))
})

test_that("checkConnect is always TRUE - it cannot report a failure|ics2050", {
  restoreSession()
  on.exit(restoreSession(), add = TRUE)

  # Both reachable states, one assertion. This block exists to pin a limitation,
  # not to praise the function: if it ever gains a FALSE path, this fails and
  # somebody has to look at IMR-290.
  healthy <- improveR::checkConnect()
  improveR::improveDisconnect()
  recovered <- improveR::checkConnect()

  expect_true(healthy)
  expect_true(recovered)
  expect_identical(healthy, recovered,
                   info = paste0("checkConnect returns the same value whether the connection ",
                                 "was valid or had to be rebuilt. If this ever differs, the ",
                                 "open question in IMR-290 has been answered and the ",
                                 "documentation must follow."))
})

test_that("improveInit sources a module and calls its init function|ics2050", {
  restoreSession()
  # improveInit calls improveConnect() itself, which resets connection state for
  # every file that runs after this one.
  on.exit(restoreSession(), add = TRUE)

  basePath <- createFolderPath("sessionLifecycle")
  folder <- improveR::createFolder(
    targetIdent = basePath,
    folderName = paste0("mod-", format(Sys.time(), "%Y%m%d%H%M%S")),
    comment = "improveInit test setup"
  )
  on.exit(tryCatch(improveR::delete(folder$resourceId),
                   error = function(e) NULL), add = TRUE)

  marker <- paste0("initialised-", uuid::UUIDgenerate())
  localPath <- file.path(tempdir(), "improveInitModule.R")
  writeLines(c(
    "improveInitModule.init <- function(rootPath) {",
    "  e <- new.env()",
    "  e$rootPath <- rootPath",
    paste0("  e$marker <- \"", marker, "\""),
    "  return(e)",
    "}"
  ), localPath)
  moduleFile <- improveR::createFile(
    targetIdent = folder$resourceId,
    localPath = localPath,
    comment = "module for improveInit"
  )
  stopifnot("the module file was not created" = !is.null(moduleFile))

  moduleEnv <- improveR::improveInit(moduleFile$resourceId)

  expect_true(is.environment(moduleEnv))
  # The init function ran: this marker exists nowhere else, and it was written
  # into the file uploaded a moment ago.
  expect_equal(moduleEnv$marker, marker)
  # And it was handed the module's FOLDER, not the file - that is the contract
  # the documentation states and the only part a caller can get wrong.
  expect_equal(as.character(moduleEnv$rootPath), as.character(folder$path))
})

test_that("improveInit returns NULL for an ident that does not resolve|ics2050", {
  restoreSession()
  on.exit(restoreSession(), add = TRUE)

  expect_null(improveR::improveInit(uuid::UUIDgenerate()))
})
