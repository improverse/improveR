# isResourceUp2Date / isFileUp2Date (ics1090, ics1099, IMR-287)
#
# Two of the thirteen exported functions the suite never entered (C9 section 7).
#
# Both are predicates, and before IMR-287 neither could be trusted to behave
# like one. Against the live server on 2026-09-15:
#
#   isResourceUp2Date(<unknown id>) -> Error: argument is of length zero
#
# which names neither the ident nor the mistake. isFileUp2Date on something that
# is not a file was worse, and measured in both of its shapes on 2026-09-15:
#
#   on an empty folder, and on a folder holding one file -> TRUE
#   on the run's own step folder                         -> Error: is.response(x) is not TRUE
#
# The TRUE is the dangerous half. No file content was compared at all: loadFile
# on a folder returns the folder's own row, so the function compares the folder
# with itself and says yes. Which of the two you get depends on what the folder
# happens to contain - and it downloads the whole tree to find out.
#
# Quieter still, a server read that failed after the local one succeeded produced
# logical(0) - neither TRUE nor FALSE nor an error.
#
# The "not up to date" case is produced by writing the file's content with a
# direct REST call, the same request improveR itself sends, minus the cache
# invalidation that follows it. That is not a trick: it is exactly the situation
# these two functions exist for - somebody else changed the resource, and the
# local copy does not know.

Sys.setenv(TEST_NAME = "up2Date")

setupUp2Date <- function() {
  Sys.setenv(TEST_NAME = "up2Date")
  improveR::improveConnect()
  improveR::setEditable(TRUE)

  basePath <- createFolderPath("up2Date")
  testFolder <- improveR::createFolder(
    targetIdent = basePath,
    folderName = paste0("u2d-", format(Sys.time(), "%Y%m%d%H%M%S")),
    comment = "up2Date test setup"
  )
  localPath <- file.path(tempdir(), paste0("u2d-", uuid::UUIDgenerate(), ".txt"))
  writeLines("first version", localPath)
  testFile <- improveR::createFile(
    targetIdent = testFolder$resourceId,
    localPath = localPath,
    comment = "up2Date target"
  )
  list(testFolder = testFolder, testFile = testFile, localPath = localPath)
}

# The same PUT singleUpdateFileContent sends, without the cache invalidation
# that follows it - so the local copy stays behind, which is the point.
writeContentBehindTheCache <- function(resourceId, text) {
  local <- file.path(tempdir(), paste0("u2d-behind-", uuid::UUIDgenerate(), ".txt"))
  writeLines(text, local)
  result <- improveR:::authenticatedREST(
    "/resources/{resourceId}/content",
    urlParams = list(resourceId = resourceId),
    queryParams = list(comment = "changed by another client"),
    data = list(file = httr::upload_file(local)),
    encode = NULL,
    restType = "PUT"
  )
  stopifnot("the direct PUT that makes the local copy stale must succeed" = !is.null(result))
  invisible(result)
}

test_that("isResourceUp2Date says TRUE for a resource nobody has touched|ics1090", {
  ctx <- setupUp2Date()
  on.exit(tryCatch(improveR::delete(ctx$testFolder$resourceId),
                   error = function(e) NULL), add = TRUE)

  up <- improveR::isResourceUp2Date(ctx$testFile$resourceId)
  expect_true(is.logical(up))
  expect_length(up, 1L)
  expect_true(up)
})

test_that("isResourceUp2Date says FALSE once somebody else has written|ics1090", {
  ctx <- setupUp2Date()
  on.exit(tryCatch(improveR::delete(ctx$testFolder$resourceId),
                   error = function(e) NULL), add = TRUE)

  # Prime the cache, so there is something to be out of date.
  expect_true(improveR::isResourceUp2Date(ctx$testFile$resourceId))

  writeContentBehindTheCache(ctx$testFile$resourceId, "second version")

  up <- improveR::isResourceUp2Date(ctx$testFile$resourceId)
  expect_length(up, 1L)
  expect_false(up)

  # And it becomes TRUE again once the local copy catches up - otherwise the
  # FALSE above could just as well mean "always FALSE after any write".
  improveR::refreshResource(ctx$testFile$resourceId)
  expect_true(improveR::isResourceUp2Date(ctx$testFile$resourceId))
})

test_that("isResourceUp2Date accepts a version id and says so|ics1090", {
  ctx <- setupUp2Date()
  on.exit(tryCatch(improveR::delete(ctx$testFolder$resourceId),
                   error = function(e) NULL), add = TRUE)

  res <- improveR::loadResource(ctx$testFile$resourceId)
  up <- improveR::isResourceUp2Date(res$entityVersionId)
  expect_length(up, 1L)
  expect_true(up)
})

test_that("isResourceUp2Date names the ident when it does not resolve|ics1090", {
  improveR::improveConnect()
  bogus <- uuid::UUIDgenerate()

  # Before IMR-287 this was "argument is of length zero", which names nothing.
  expect_error(improveR::isResourceUp2Date(bogus),
               regexp = "isResourceUp2Date", fixed = FALSE)
  expect_error(improveR::isResourceUp2Date(bogus),
               regexp = "does not resolve", fixed = FALSE)
  expect_error(improveR::isResourceUp2Date(bogus),
               regexp = bogus, fixed = TRUE)
})

test_that("isFileUp2Date says TRUE, then FALSE after a write behind the cache|ics1099", {
  ctx <- setupUp2Date()
  on.exit(tryCatch(improveR::delete(ctx$testFolder$resourceId),
                   error = function(e) NULL), add = TRUE)

  up <- improveR::isFileUp2Date(ctx$testFile$resourceId)
  expect_length(up, 1L)
  expect_true(up)

  writeContentBehindTheCache(ctx$testFile$resourceId, "second version")

  stale <- improveR::isFileUp2Date(ctx$testFile$resourceId)
  expect_length(stale, 1L)
  expect_false(stale)
})

test_that("isFileUp2Date refuses a resource that is not a file|ics1099", {
  ctx <- setupUp2Date()
  on.exit(tryCatch(improveR::delete(ctx$testFolder$resourceId),
                   error = function(e) NULL), add = TRUE)

  # Before IMR-287 this reached httr and came back as
  # "is.response(x) is not TRUE".
  expect_error(improveR::isFileUp2Date(ctx$testFolder$resourceId),
               regexp = "not a file", fixed = FALSE)
  expect_error(improveR::isFileUp2Date(ctx$testFolder$resourceId),
               regexp = "Folder", fixed = TRUE)
})

test_that("isFileUp2Date names the ident when it does not resolve|ics1099", {
  improveR::improveConnect()
  bogus <- uuid::UUIDgenerate()
  expect_error(improveR::isFileUp2Date(bogus),
               regexp = "isFileUp2Date", fixed = FALSE)
  expect_error(improveR::isFileUp2Date(bogus),
               regexp = bogus, fixed = TRUE)
})
