# The five exported functions the suite had never entered (IMR-304)
#
# These were the whole remaining coverage gap after the deprecated aliases
# (IMR-301). Each was excluded for a reason that was true of a LIVE test and is
# not true of a mocked one:
#
#   updateAccessToken           "with an invented token the test breaks every
#                                file that follows; with the real one it asserts
#                                nothing"
#   refreshMetaDataDefinitions  "repository-wide configuration. Unloading or
#   unloadMetaDataDefinitions    refreshing them affects every file that follows"
#   updateMetaDateById
#
# Both objections are about damage to shared state on a live server. There is no
# shared state here: the cache and the REST layer are replaced, and every
# environment variable the token writer touches is saved and put back. Nothing
# reaches the network - verified with --network none.
#
# improveOAuth is deliberately NOT in this file. Every run already exercises it
# and 24 preconditions assert the session it produces; what it needs is a
# recorded exchange, which belongs with the replay package and not here.

# ---------------------------------------------------------------------------
# updateAccessToken - the write half of a refresh cycle
# ---------------------------------------------------------------------------

# Saves and restores every variable the function writes. This is the entire
# reason the function could not be tested live: it writes the session's token,
# and a test that leaves an invented one behind breaks every file after it.
withTokenEnvRestored <- function(code) {
  vars  <- c("IMPROVER_TOKEN", "IMPROVER_TOKEN_EXPIRATION",
             "IMPROVER_REFRESH_TOKEN", "IMPROVER_LAST_ACCESS")
  saved <- Sys.getenv(vars, names = TRUE, unset = NA)
  on.exit({
    for (v in vars) {
      if (is.na(saved[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, setNames(list(saved[[v]]), v))
    }
  }, add = TRUE)
  force(code)
}

test_that("updateAccessToken refuses a token that is not one|ics2071,IMR-304", {
  withTokenEnvRestored({
    expect_error(improveR::updateAccessToken(NULL), "Valid token is required")
    expect_error(improveR::updateAccessToken(""),   "Valid token is required")
  })
})

test_that("updateAccessToken writes the token and only the fields it was given|ics2071,IMR-304", {
  withTokenEnvRestored({
    Sys.setenv(IMPROVER_TOKEN_EXPIRATION = "sentinel-expiration",
               IMPROVER_REFRESH_TOKEN    = "sentinel-refresh")

    expect_true(improveR::updateAccessToken("token-alpha"))
    expect_equal(Sys.getenv("IMPROVER_TOKEN"), "token-alpha")

    # not passed, so not touched
    expect_equal(Sys.getenv("IMPROVER_TOKEN_EXPIRATION"), "sentinel-expiration")
    expect_equal(Sys.getenv("IMPROVER_REFRESH_TOKEN"),    "sentinel-refresh")

    # lastAccess has no "leave it alone" branch: omitted, it is set to now
    expect_true(nzchar(Sys.getenv("IMPROVER_LAST_ACCESS")))
  })
})

test_that("updateAccessToken writes expiration, refresh token and lastAccess when given|ics2071,IMR-304", {
  withTokenEnvRestored({
    expiresAt  <- 1789123456
    lastAccess <- 1788123456

    expect_true(improveR::updateAccessToken("token-beta",
                                            expiration   = expiresAt,
                                            refreshToken = "refresh-beta",
                                            lastAccess   = lastAccess))
    expect_equal(Sys.getenv("IMPROVER_TOKEN"),         "token-beta")
    expect_equal(Sys.getenv("IMPROVER_REFRESH_TOKEN"), "refresh-beta")

    # The VALUE, not its formatting. The function writes these with
    # as.character(), and R renders a round number in scientific notation when
    # that is shorter - as.character(1789000000) is "1.789e+09". Every reader
    # takes them back through as.numeric() (tokenManagement.R:142-143), so the
    # value round-trips exactly and the spelling does not matter. An earlier
    # version of this test asserted the spelling and failed on numbers that
    # happened to be round; that was the test being wrong, not the code.
    expect_equal(as.numeric(Sys.getenv("IMPROVER_TOKEN_EXPIRATION")), expiresAt)
    expect_equal(as.numeric(Sys.getenv("IMPROVER_LAST_ACCESS")),      lastAccess)
  })
})

test_that("a round expiration still round-trips through the environment|ics2071,IMR-304", {
  # The case that exposed the formatting: as.character(1789000000) is
  # "1.789e+09". Pinned so that a future change to how these are written cannot
  # quietly break the readers.
  withTokenEnvRestored({
    expect_true(improveR::updateAccessToken("token-round", expiration = 1789000000))
    expect_equal(as.numeric(Sys.getenv("IMPROVER_TOKEN_EXPIRATION")), 1789000000)
  })
})

test_that("updateAccessToken also updates the cached configuration|ics2071,IMR-304", {
  # The cached conf carries reqToken separately from the environment. If the two
  # disagree, the next request authenticates with the old token while the
  # environment says otherwise - the kind of split this release keeps finding.
  withTokenEnvRestored({
    cacheEnv <- get("cacheEnv", envir = asNamespace("improveR"))
    saved    <- cacheEnv$conf
    on.exit(cacheEnv$conf <- saved, add = TRUE)

    cacheEnv$conf <- list(repoUrl = "https://example.invalid/repository",
                          reqToken = "token-old")
    expect_true(improveR::updateAccessToken("token-new"))
    expect_equal(cacheEnv$conf$reqToken, "token-new")
    expect_equal(cacheEnv$conf$repoUrl, "https://example.invalid/repository")
  })
})

# ---------------------------------------------------------------------------
# The metadata definition cache
# ---------------------------------------------------------------------------

test_that("unloadMetaDataDefinitions clears the definitions cache for its scope|ics1137,IMR-304", {
  seen <- NULL
  testthat::with_mocked_bindings(
    improveR::unloadMetaDataDefinitions("Improve Client"),
    removeFromCache = function(key, argument, cacheList) {
      seen <<- list(key = key, argument = argument, cacheList = cacheList)
      invisible(NULL)
    },
    .package = "improveR"
  )
  expect_equal(seen$key, "Improve Client")
  expect_equal(seen$argument, "")
  expect_identical(seen$cacheList,
                   get("metadataDefinitionsCacheList", envir = asNamespace("improveR")))
})

test_that("unloadMetaDataDefinitions defaults to the Improve Client scope|ics1137,IMR-304", {
  seen <- NULL
  testthat::with_mocked_bindings(
    improveR::unloadMetaDataDefinitions(),
    removeFromCache = function(key, argument, cacheList) {
      seen <<- key; invisible(NULL)
    },
    .package = "improveR"
  )
  expect_equal(seen, "Improve Client")
})

test_that("refreshMetaDataDefinitions unloads before it loads|ics1137,IMR-304", {
  order <- character(0)
  result <- testthat::with_mocked_bindings(
    improveR::refreshMetaDataDefinitions("Some Scope"),
    unloadMetaDataDefinitions = function(scope) {
      order <<- c(order, paste0("unload:", scope)); invisible(NULL)
    },
    loadMetaDataDefinitions = function(scope) {
      order <<- c(order, paste0("load:", scope)); "DEFINITIONS"
    },
    .package = "improveR"
  )
  # The order is the whole behaviour: loading first would repopulate the cache
  # from the value that was about to be thrown away.
  expect_equal(order, c("unload:Some Scope", "load:Some Scope"))
  expect_equal(result, "DEFINITIONS")
})

test_that("refreshMetaDataDefinitions returns what the load returned|ics1137,IMR-304", {
  result <- testthat::with_mocked_bindings(
    improveR::refreshMetaDataDefinitions(),
    unloadMetaDataDefinitions = function(scope) invisible(NULL),
    loadMetaDataDefinitions   = function(scope) NULL,
    .package = "improveR"
  )
  expect_null(result)
})

# ---------------------------------------------------------------------------
# updateMetaDateById
# ---------------------------------------------------------------------------

test_that("updateMetaDateById multiplexes over the identifiers it is given|IMR-304", {
  seen <- NULL
  result <- testthat::with_mocked_bindings(
    improveR::updateMetaDateById(c("ident-one", "ident-two"),
                                 metadataId = "MD-1", value = "2026-01-01"),
    multiplexResourceFunction = function(func, multiArgument, ...) {
      seen <<- list(func = func, multiArgument = multiArgument, rest = list(...))
      "MULTIPLEXED"
    },
    .package = "improveR"
  )
  expect_equal(result, "MULTIPLEXED")
  expect_equal(seen$multiArgument, c("ident-one", "ident-two"))
  expect_equal(seen$rest$metadataId, "MD-1")
  expect_equal(seen$rest$value, "2026-01-01")
  expect_identical(seen$func,
                   get("singleUpdateMetaDateById", envir = asNamespace("improveR")))
})

test_that("updateMetaDateById passes the single-resource worker, which exists|IMR-304", {
  # The IMR-288 check: the function it delegates to must be a function.
  ns <- asNamespace("improveR")
  expect_true(exists("singleUpdateMetaDateById", envir = ns, inherits = FALSE))
  expect_true(is.function(get0("singleUpdateMetaDateById", envir = ns)))
})
