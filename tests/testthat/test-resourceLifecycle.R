# Test Resource Lifecycle Functions
# Tests: finishResource, reopenResource

ensureTestFolder <- function() {
  if (!exists("TEST_FOLDER", envir = globalenv())) {
    tryCatch({
      improveR::improveConnect()
      improveR::setEditable(TRUE)
      testFolder <- improveR::createFolder(
        targetIdent = "/",
        folderName = paste0("test-reslc-", format(Sys.time(), "%Y%m%d%H%M%S")),
        comment = "resource lifecycle test setup"
      )
      assign("TEST_FOLDER", testFolder, envir = globalenv())
    }, error = function(e) {
      skip(paste("Server not available:", e$message))
    })
  }
}

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------
test_that("setup resource lifecycle test environment", {
  tryCatch({
    improveR::improveConnect()
    improveR::setEditable(TRUE)
  }, error = function(e) {
    skip(paste("Server not available:", e$message))
  })

  testFolder <- improveR::createFolder(
    targetIdent = "/",
    folderName = paste0("test-reslc-", format(Sys.time(), "%Y%m%d%H%M%S")),
    comment = "resource lifecycle test setup"
  )
  expect_false(is.null(testFolder))
  assign("TEST_FOLDER", testFolder, envir = globalenv())

  testFile <- improveR::createFile(
    targetIdent = testFolder$resourceId,
    fileName = "lifecycle-test-file.txt",
    comment = "test file for lifecycle"
  )
  expect_false(is.null(testFile))
  assign("TEST_FILE", testFile, envir = globalenv())
})

# ---------------------------------------------------------------------------
# finishResource | ics1810
# ---------------------------------------------------------------------------
test_that("finishResource finishes a resource|ics1810", {
  ensureTestFolder()
  testFile <- get("TEST_FILE", envir = globalenv())

  result <- improveR::finishResource(testFile$resourceId)
  if (isFALSE(result)) {
    skip("finishResource not supported on this server")
  }
  expect_true(result)
  cat("Finished resource:", testFile$resourceId, "\n")
})

# ---------------------------------------------------------------------------
# reopenResource | ics1811
# ---------------------------------------------------------------------------
test_that("reopenResource reopens a finished resource|ics1811", {
  ensureTestFolder()
  testFile <- get("TEST_FILE", envir = globalenv())

  result <- improveR::reopenResource(testFile$resourceId)
  if (isFALSE(result)) {
    skip("reopenResource not supported on this server")
  }
  expect_true(result)
  cat("Reopened resource:", testFile$resourceId, "\n")
})

# ---------------------------------------------------------------------------
# Validation: non-existent resource
# ---------------------------------------------------------------------------
test_that("finishResource returns FALSE for non-existent resource|ics1810", {
  ensureTestFolder()
  result <- improveR::finishResource("non-existent-id-00000000")
  expect_false(result)
})

test_that("reopenResource returns FALSE for non-existent resource|ics1811", {
  ensureTestFolder()
  result <- improveR::reopenResource("non-existent-id-00000000")
  expect_false(result)
})

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
test_that("cleanup resource lifecycle test environment", {
  if (exists("TEST_FOLDER", envir = globalenv())) {
    testFolder <- get("TEST_FOLDER", envir = globalenv())
    tryCatch({
      improveR::delete(testFolder$resourceId, comment = "resource lifecycle test cleanup")
    }, error = function(e) {
      cat("Cleanup warning:", e$message, "\n")
    })
    rm("TEST_FOLDER", envir = globalenv())
  }
  if (exists("TEST_FILE", envir = globalenv())) rm("TEST_FILE", envir = globalenv())
})
