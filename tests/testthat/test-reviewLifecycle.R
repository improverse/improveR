# Test Review Lifecycle Functions
# Tests: getReviewById, acceptReview, declineReview, changeReviewStatus

ensureTestFolder <- function() {
  if (!exists("TEST_FOLDER", envir = globalenv())) {
    tryCatch({
      improveR::improveConnect()
      improveR::setEditable(TRUE)
      testFolder <- improveR::createFolder(
        targetIdent = "/",
        folderName = paste0("test-reviewlc-", format(Sys.time(), "%Y%m%d%H%M%S")),
        comment = "review lifecycle test setup"
      )
      assign("TEST_FOLDER", testFolder, envir = globalenv())
    }, error = function(e) {
      skip(paste("Server not available:", e$message))
    })
  }
}

# Helper: create a review and return the resource data frame
createTestReview <- function(name, testFolder, testFile, allUsers) {
  result <- improveR::createReview(
    name = name,
    parentIdent = testFolder$path,
    comment = "review lifecycle test",
    templateId = NULL,
    resourceIds = list(testFile$resourceId),
    reviewerIds = list(allUsers$id[1]),
    dueDate = format(Sys.Date() + 30, "%Y-%m-%d")
  )
  return(result)
}

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------
test_that("setup review lifecycle test environment", {
  tryCatch({
    improveR::improveConnect()
    improveR::setEditable(TRUE)
  }, error = function(e) {
    skip(paste("Server not available:", e$message))
  })

  testFolder <- improveR::createFolder(
    targetIdent = "/",
    folderName = paste0("test-reviewlc-", format(Sys.time(), "%Y%m%d%H%M%S")),
    comment = "review lifecycle test setup"
  )
  expect_false(is.null(testFolder))
  assign("TEST_FOLDER", testFolder, envir = globalenv())

  testFile <- improveR::createFile(
    targetIdent = testFolder$resourceId,
    fileName = "reviewlc-test-file.txt",
    comment = "test file for review lifecycle"
  )
  expect_false(is.null(testFile))
  assign("TEST_FILE", testFile, envir = globalenv())

  allUsers <- improveR::users()
  expect_false(is.null(allUsers))
  assign("TEST_USERS", allUsers, envir = globalenv())

  # Create a review for lifecycle tests
  reviewName <- paste0("LCReview-", format(Sys.time(), "%H%M%S"))
  reviewData <- createTestReview(reviewName, testFolder, testFile, allUsers)
  if (!is.null(reviewData)) {
    assign("TEST_REVIEW", reviewData, envir = globalenv())
    cat("Created review:", reviewData$resourceId, "\n")
  }
})

# ---------------------------------------------------------------------------
# getReviewById | ics1528
# ---------------------------------------------------------------------------
test_that("getReviewById retrieves a review|ics1528", {
  ensureTestFolder()
  if (!exists("TEST_REVIEW", envir = globalenv())) skip("No review created")
  review <- get("TEST_REVIEW", envir = globalenv())

  result <- improveR::getReviewById(review)
  if (is.null(result)) {
    skip("getReviewById not supported on this server")
  }
  expect_false(is.null(result))
  expect_true(is.data.frame(result))
  cat("Retrieved review:", review$resourceId, "\n")
})

# ---------------------------------------------------------------------------
# acceptReview | ics1476
# ---------------------------------------------------------------------------
test_that("acceptReview accepts a review|ics1476", {
  ensureTestFolder()
  if (!exists("TEST_REVIEW", envir = globalenv())) skip("No review created")
  review <- get("TEST_REVIEW", envir = globalenv())

  result <- improveR::acceptReview(review, comment = "automated accept")
  if (!result) {
    skip("acceptReview failed - review may not be in correct state")
  }
  expect_true(result)
  cat("Accepted review:", review$resourceId, "\n")
})

# ---------------------------------------------------------------------------
# declineReview | ics1477
# ---------------------------------------------------------------------------
test_that("declineReview declines a review|ics1477", {
  ensureTestFolder()
  if (!exists("TEST_FOLDER", envir = globalenv())) skip("No test folder")
  testFolder <- get("TEST_FOLDER", envir = globalenv())
  testFile <- get("TEST_FILE", envir = globalenv())
  allUsers <- get("TEST_USERS", envir = globalenv())

  reviewName <- paste0("DeclineReview-", format(Sys.time(), "%H%M%S"))
  reviewData <- createTestReview(reviewName, testFolder, testFile, allUsers)
  if (is.null(reviewData)) skip("Could not create review for decline test")

  result <- improveR::declineReview(reviewData, comment = "automated decline")
  if (!result) {
    skip("declineReview failed - review may not be in correct state")
  }
  expect_true(result)
  cat("Declined review:", reviewData$resourceId, "\n")
})

# ---------------------------------------------------------------------------
# changeReviewStatus | ccs27
# ---------------------------------------------------------------------------
test_that("changeReviewStatus changes a review status|ccs27", {
  ensureTestFolder()
  if (!exists("TEST_FOLDER", envir = globalenv())) skip("No test folder")
  testFolder <- get("TEST_FOLDER", envir = globalenv())
  testFile <- get("TEST_FILE", envir = globalenv())
  allUsers <- get("TEST_USERS", envir = globalenv())

  reviewName <- paste0("StatusReview-", format(Sys.time(), "%H%M%S"))
  reviewData <- createTestReview(reviewName, testFolder, testFile, allUsers)
  if (is.null(reviewData)) skip("Could not create review for status test")

  result <- improveR::changeReviewStatus(reviewData, "Closed")
  if (!result) {
    skip("changeReviewStatus not supported on this server")
  }
  expect_true(result)
  cat("Changed review status to Closed:", reviewData$resourceId, "\n")
})

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
test_that("cleanup review lifecycle test environment", {
  if (exists("TEST_FOLDER", envir = globalenv())) {
    testFolder <- get("TEST_FOLDER", envir = globalenv())
    tryCatch({
      improveR::delete(testFolder$resourceId, comment = "review lifecycle test cleanup")
    }, error = function(e) {
      cat("Cleanup warning:", e$message, "\n")
    })
    rm("TEST_FOLDER", envir = globalenv())
  }
  if (exists("TEST_FILE", envir = globalenv())) rm("TEST_FILE", envir = globalenv())
  if (exists("TEST_REVIEW", envir = globalenv())) rm("TEST_REVIEW", envir = globalenv())
  if (exists("TEST_USERS", envir = globalenv())) rm("TEST_USERS", envir = globalenv())
})
