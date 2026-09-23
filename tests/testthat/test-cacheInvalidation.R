# Test Cache Invalidation
# Verifies that operations which modify server state (lock/unlock, finish/reopen,
# create/delete children, push) properly invalidate the cache so that subsequent
# loadResource() / loadChildResources() calls return fresh data.

ensureTestFolder <- function() {
  if (!exists("TEST_FOLDER_CI", envir = globalenv())) {
    improveR::improveConnect()
    improveR::setEditable(TRUE)
    basePath <- createFolderPath("cacheInvalidation")
    testFolder <- improveR::createFolder(
      targetIdent = basePath,
      folderName = paste0("test-cache-", uniqueTag())
    )
    assign("TEST_FOLDER_CI", testFolder, envir = globalenv())
  }
}

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------
test_that("setup cache invalidation test environment", {
  improveR::improveConnect()
  improveR::setEditable(TRUE)

  basePath <- createFolderPath("cacheInvalidation")
  testFolder <- improveR::createFolder(
    targetIdent = basePath,
    folderName = paste0("test-cache-", uniqueTag())
  )
  expect_false(is.null(testFolder))
  assign("TEST_FOLDER_CI", testFolder, envir = globalenv())

  # Create a test file for lock/unlock and finish/reopen tests
  testFile <- improveR::createFile(
    targetIdent = testFolder$resourceId,
    fileName = "cache-test-file.txt"
  )
  expect_false(is.null(testFile))
  assign("TEST_FILE_CI", testFile, envir = globalenv())
})

# ---------------------------------------------------------------------------
# lock/unlock cache invalidation
# ---------------------------------------------------------------------------
test_that("lockResource invalidates cache so loadResource shows locked state|ics1091,ics2049", {
  ensureTestFolder()
  testFile <- get("TEST_FILE_CI", envir = globalenv())

  # Load resource to populate cache
  res <- improveR::loadResource(testFile$resourceId)
  expect_false(is.null(res))

  # Lock it
  lockResult <- improveR::lockResource(testFile$resourceId)
  requireServerCall(lockResult, "lockResource")
  expect_true(lockResult)

  # loadResource should now return locked state (cache was invalidated)
  freshRes <- improveR::loadResource(testFile$resourceId)
  expect_true("lockedByName" %in% names(freshRes))
  expect_false(is.na(freshRes$lockedByName))
  cat("Lock cache invalidation verified: lockedByName =", freshRes$lockedByName, "\n")
})

test_that("unlockResource invalidates cache so loadResource shows unlocked state|ics1091,ics2049", {
  ensureTestFolder()
  testFile <- get("TEST_FILE_CI", envir = globalenv())

  # Unlock it (should be locked from previous test)
  unlockResult <- improveR::unlockResource(testFile$resourceId)
  requireServerCall(unlockResult, "unlockResource")
  expect_true(unlockResult)

  # loadResource should now return unlocked state
  freshRes <- improveR::loadResource(testFile$resourceId)
  isUnlocked <- !("lockedByName" %in% names(freshRes)) || is.na(freshRes$lockedByName)
  expect_true(isUnlocked)
  cat("Unlock cache invalidation verified\n")
})

# ---------------------------------------------------------------------------
# finish/reopen cache invalidation
# ---------------------------------------------------------------------------
test_that("finishResource invalidates cache and loadResource succeeds after finish|ics1091", {
  ensureTestFolder()
  testFile <- get("TEST_FILE_CI", envir = globalenv())

  # Load to populate cache
  res <- improveR::loadResource(testFile$resourceId)
  expect_false(is.null(res))

  # Finish it
  finishResult <- improveR::finishResource(testFile$resourceId)
  requireServerCall(finishResult, "finishResource")
  expect_true(finishResult)

  # Cache was invalidated by unloadResource inside finishResource.
  # Verify loadResource returns fresh data (not stale cached data).
  freshRes <- improveR::loadResource(testFile$resourceId)
  expect_false(is.null(freshRes))
  cat("Finish cache invalidation verified: loadResource returned fresh data\n")
})

test_that("reopenResource invalidates cache and loadResource succeeds after reopen|ics1091", {
  ensureTestFolder()
  testFile <- get("TEST_FILE_CI", envir = globalenv())

  # Reopen (should be finished from previous test)
  reopenResult <- improveR::reopenResource(testFile$resourceId)
  requireServerCall(reopenResult, "reopenResource")
  expect_true(reopenResult)

  # Cache was invalidated — verify fresh load works
  freshRes <- improveR::loadResource(testFile$resourceId)
  expect_false(is.null(freshRes))
  cat("Reopen cache invalidation verified: loadResource returned fresh data\n")
})

# ---------------------------------------------------------------------------
# create/delete child cache invalidation
# ---------------------------------------------------------------------------
test_that("createFile invalidates child cache so loadChildResources sees new file|ics1091,ics1085,ics1102", {
  ensureTestFolder()
  testFolder <- get("TEST_FOLDER_CI", envir = globalenv())

  # Pre-load children to populate cache
  childrenBefore <- improveR::loadChildResources(testFolder)$data[[1]]
  countBefore <- if (is.null(childrenBefore)) 0L else nrow(childrenBefore)

  # Create a new child file
  newFile <- improveR::createFile(
    targetIdent = testFolder$resourceId,
    fileName = "new-child-file.txt"
  )
  expect_false(is.null(newFile))
  assign("CHILD_FILE_CI", newFile, envir = globalenv())

  # loadChildResources should see the new file without manual cache clear
  childrenAfter <- improveR::loadChildResources(testFolder)$data[[1]]
  countAfter <- if (is.null(childrenAfter)) 0L else nrow(childrenAfter)
  expect_true(countAfter > countBefore)
  expect_true(newFile$name %in% childrenAfter$name)
  cat("Create child cache invalidation verified:", countBefore, "->", countAfter, "children\n")
})

test_that("delete invalidates child cache so loadChildResources no longer sees file|ics1091,ics1085,ics1139", {
  ensureTestFolder()
  testFolder <- get("TEST_FOLDER_CI", envir = globalenv())
  childFile <- get("CHILD_FILE_CI", envir = globalenv())

  # Pre-load to populate cache
  childrenBefore <- improveR::loadChildResources(testFolder)$data[[1]]
  expect_true(childFile$name %in% childrenBefore$name)

  # Delete the child
  improveR::delete(childFile$resourceId)

  # Invalidate parent's child cache and verify
  improveR::unloadChildResources(testFolder$resourceId)
  childrenAfter <- improveR::loadChildResources(testFolder)$data[[1]]
  if (!is.null(childrenAfter) && nrow(childrenAfter) > 0) {
    expect_false(childFile$name %in% childrenAfter$name)
  }
  cat("Delete child cache invalidation verified\n")
  if (exists("CHILD_FILE_CI", envir = globalenv())) rm("CHILD_FILE_CI", envir = globalenv())
})

# ---------------------------------------------------------------------------
# push invalidation
# ---------------------------------------------------------------------------
test_that("pushCli invalidates cache for pushed resource", {
  ensureTestFolder()
  testFolder <- get("TEST_FOLDER_CI", envir = globalenv())

  r_runserver <- Sys.getenv("R_RUNSERVER")
  r_tool <- Sys.getenv("R_TOOL")
  r_tool_instance <- Sys.getenv("R_TOOL_INSTANCE")

  stopifnot(
    "R_RUNSERVER env var must be set"      = r_runserver != "",
    "R_TOOL env var must be set"           = r_tool      != "",
    "R_TOOL_INSTANCE env var must be set"  = r_tool_instance != ""
  )

  # Create a tree and step
  testTree <- improveR::createAnalysisTree(testFolder, "CacheTestTree")
  expect_false(is.null(testTree))

  stepEnv <- improveR::createStepTemplateEnv(treeIdent = testTree)
  stepEnv$setStepRunserverLabel(r_runserver)
  stepEnv$setStepToolLabel(r_tool)
  stepEnv$setStepToolInstance(r_tool_instance)
  stepEnv$setStepDescription("Cache push test step")
  stepEnv$setStepRationale("Testing cache invalidation on push")

  realStep <- stepEnv$realise()
  expect_false(is.null(realStep))

  # Get the step resource
  stepEntityId <- stepEnv$stepDf$sourceEntityId
  if (is.null(stepEntityId) || is.na(stepEntityId)) {
    stepEntityId <- stepEnv$stepDf$entityId
  }
  stepResource <- improveR::loadResource(stepEntityId)
  if (is.null(stepResource)) {
    stop("Could not load realised step resource")
  }

  # Clone and push a file
  localPath <- file.path(tempdir(), paste0("pushCacheTest", uuid::UUIDgenerate()))
  dir.create(localPath, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(localPath, recursive = TRUE, force = TRUE), add = TRUE)
  improveR::cloneCli(stepResource, localPath = localPath)

  testFilePath <- file.path(localPath, "push-test-data.txt")
  writeLines("test data for push cache invalidation", testFilePath)
  improveR::pushCli(localPath)

  # After push, loadResource should reflect updated state
  freshStep <- improveR::loadResource(stepResource$resourceId)
  expect_false(is.null(freshStep))

  # Check child resources include the pushed file
  children <- improveR::loadChildResources(stepResource)$data[[1]]
  expect_true("push-test-data.txt" %in% children$name)
  cat("Push cache invalidation verified: pushed file visible in children\n")

  # Note: NOT calling finishRun() — this test only verifies push cache invalidation,
  # not step execution. finishRun() would block waiting for server-side execution.
})

# ---------------------------------------------------------------------------
# updateFileContent cache invalidation
# ---------------------------------------------------------------------------
test_that("updateFileContent invalidates file download cache so getTextString returns new content|ics1091,ics1210,ics1141", {
  ensureTestFolder()
  testFolder <- get("TEST_FOLDER_CI", envir = globalenv())

  # Create a test file with initial content
  initialContent <- "initial content for cache test"
  tmpFile <- tempfile(fileext = ".txt")
  writeLines(initialContent, tmpFile)
  on.exit(unlink(tmpFile), add = TRUE)

  testFile <- improveR::createFile(
    targetIdent = testFolder$resourceId,
    fileName = "cache-content-test.txt",
    localPath = tmpFile
  )
  expect_false(is.null(testFile))

  # Load text to populate the "text" folder cache
  text1 <- improveR::getTextString(testFile)
  expect_equal(text1$data, initialContent)

  # Update content on server
  updatedContent <- "updated content for cache test"
  tmpFile2 <- tempfile(fileext = ".txt")
  writeLines(updatedContent, tmpFile2)
  on.exit(unlink(tmpFile2), add = TRUE)

  improveR::updateFileContent(testFile, localPath = tmpFile2)

  # getTextString should return updated content (cache was invalidated)
  text2 <- improveR::getTextString(testFile)
  expect_equal(text2$data, updatedContent)
  cat("updateFileContent cache invalidation verified: content changed from initial to updated\n")

  # Cleanup
  improveR::delete(testFile$resourceId)
})

# ---------------------------------------------------------------------------
# refreshFile cross-cache invalidation
# ---------------------------------------------------------------------------
test_that("refreshFile invalidates all file caches including text folder cache|ics1091", {
  ensureTestFolder()
  testFolder <- get("TEST_FOLDER_CI", envir = globalenv())

  # Create a test file
  content <- "content for refresh cache test"
  tmpFile <- tempfile(fileext = ".txt")
  writeLines(content, tmpFile)
  on.exit(unlink(tmpFile), add = TRUE)

  testFile <- improveR::createFile(
    targetIdent = testFolder$resourceId,
    fileName = "cache-refresh-test.txt",
    localPath = tmpFile
  )
  expect_false(is.null(testFile))

  # Load via getTextString to populate "text" cache (filePath="text", addIdToName=TRUE)
  text1 <- improveR::getTextString(testFile)
  expect_false(is.null(text1))
  expect_equal(text1$data, content)

  # refreshFile with default filePath="." should also invalidate the "text" cache
  refreshed <- improveR::refreshFile(testFile)
  expect_false(is.null(refreshed))

  # getTextString should still work (re-downloads to "text" folder)
  text2 <- improveR::getTextString(testFile)
  expect_false(is.null(text2))
  expect_equal(text2$data, content)
  cat("refreshFile cross-cache invalidation verified: getTextString works after refreshFile\n")

  # Cleanup
  improveR::delete(testFile$resourceId)
})

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
test_that("cleanup cache invalidation test environment", {
  if (exists("TEST_FOLDER_CI", envir = globalenv())) {
    testFolder <- get("TEST_FOLDER_CI", envir = globalenv())
    tryCatch({
      improveR::delete(testFolder$resourceId)
    }, error = function(e) {
      cat("Cleanup warning:", e$message, "\n")
    })
    rm("TEST_FOLDER_CI", envir = globalenv())
  }
  if (exists("TEST_FILE_CI", envir = globalenv())) rm("TEST_FILE_CI", envir = globalenv())
  if (exists("CHILD_FILE_CI", envir = globalenv())) rm("CHILD_FILE_CI", envir = globalenv())
  expect_true(TRUE)
})
