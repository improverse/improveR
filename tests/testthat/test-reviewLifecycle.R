# Test Review Lifecycle Functions
# Tests: getReviewById, acceptReviewInvitation, declineReviewInvitation, changeReviewStatus
#
# Review state machine: Open -> Reviewing -> Accepted/Declined
# accept/decline require "Reviewing" state, which needs changeReviewStatus.
# Reviews are created as admin with test1 as reviewer. connectAs("test1") is
# used where the reviewer identity is needed.

hasConnectAs <- function() {
  "improveRtestsupport" %in% loadedNamespaces() &&
    exists("connectAs", envir = asNamespace("improveRtestsupport"))
}

# Reconnects as the identity the run was started with, not as a hardcoded
# "admin". The previous version called connectAs("admin") - whose password
# argument defaulted to the username - and its error branch set
# IMPROVER_TEST_USERNAME/PASSWORD to "admin"/"admin" directly. Both put
# credentials in test code, and both made the outcome of this file depend on
# which file had run before it (IMR-267).
reconnectAsRunUser <- function() {
  improveRtestsupport::connectAsRunUser()
  improveR::setEditable(TRUE)
}

# Helper: find the test1 user ID from the users list
getTest1UserId <- function(allUsers) {
  idx <- which(allUsers$username == "test1")
  if (length(idx) == 0) return(NULL)
  allUsers$id[idx[1]]
}

# Helper: create a review with test1 as reviewer
createTestReview <- function(name, testFolder, testFile, test1UserId) {
  improveR::createReview(
    name = name,
    parentIdent = testFolder$path,
    comment = "review lifecycle test",
    templateId = NULL,
    resourceIds = list(testFile$resourceId),
    reviewerIds = list(test1UserId),
    dueDate = format(runDate() + 30, "%Y-%m-%d")
  )
}

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------
test_that("setup review lifecycle test environment", {
  stopifnot("multi-user testbed required (hasConnectAs() must be TRUE)" = hasConnectAs())

  tryCatch({
    improveR::improveConnect()
    improveR::setEditable(TRUE)
  }, error = function(e) {
    skip(paste("Server not available:", e$message))
  })

  basePath <- createFolderPath("reviewLifecycle")
  testFolder <- improveR::createFolder(
    targetIdent = basePath,
    folderName = paste0("test-reviewlc-", uniqueTag()),
    comment = "review lifecycle test setup"
  )
  expect_false(is.null(testFolder))
  assign("REVLC_FOLDER", testFolder, envir = globalenv())

  testFile <- improveR::createFile(
    targetIdent = testFolder$resourceId,
    fileName = "reviewlc-test-file.txt",
    comment = "test file for review lifecycle"
  )
  expect_false(is.null(testFile))
  assign("REVLC_FILE", testFile, envir = globalenv())

  allUsers <- improveR::users()
  expect_false(is.null(allUsers))

  test1Id <- getTest1UserId(allUsers)
  stopifnot("test1 user not found on this server" = !is.null(test1Id))
  assign("REVLC_TEST1_ID", test1Id, envir = globalenv())

  # Create a review as admin with test1 as reviewer
  reviewName <- paste0("LCReview-", uniqueTag(6))
  reviewData <- createTestReview(reviewName, testFolder, testFile, test1Id)
  expect_false(is.null(reviewData))
  assign("REVLC_REVIEW", reviewData, envir = globalenv())
  cat("Created review:", reviewData$resourceId, "with test1 as reviewer\n")
})

# ---------------------------------------------------------------------------
# getReviewById | ics1528
# ---------------------------------------------------------------------------
test_that("getReviewById retrieves a review|ics1528", {
  stopifnot("No review created" = exists("REVLC_REVIEW", envir = globalenv()))
  review <- get("REVLC_REVIEW", envir = globalenv())

  result <- improveR::getReviewById(review)
  expect_false(is.null(result))
  expect_true(is.data.frame(result))
  cat("Retrieved review:", review$resourceId, "\n")
})

# ---------------------------------------------------------------------------
# acceptReviewInvitation | ics1476
# Transition: Open -> Reviewing -> Accepted
# ---------------------------------------------------------------------------
test_that("acceptReviewInvitation accepts a review|ics1476,ics2045", {
  stopifnot("No test folder" = exists("REVLC_FOLDER", envir = globalenv()))
  stopifnot("multi-user testbed required (hasConnectAs() must be TRUE)" = hasConnectAs())
  testFolder <- get("REVLC_FOLDER", envir = globalenv())
  testFile <- get("REVLC_FILE", envir = globalenv())
  test1Id <- get("REVLC_TEST1_ID", envir = globalenv())

  # Create a fresh review for accept test
  reviewName <- paste0("AcceptReview-", uniqueTag(6))
  reviewData <- createTestReview(reviewName, testFolder, testFile, test1Id)
  expect_false(is.null(reviewData))

  # Transition to Reviewing state
  statusOk <- improveR::changeReviewStatus(reviewData, "Reviewing")
  expect_true(statusOk, info = "Could not transition review to Reviewing state")

  # Switch to test1 (the reviewer) to accept
  improveRtestsupport::connectAs("test1")
  improveR::setEditable(TRUE)

  result <- improveR::acceptReviewInvitation(reviewData, comment = "automated accept by test1")
  expect_true(result)
  cat("Accepted review:", reviewData$resourceId, "\n")

  # Switch back to admin
  reconnectAsRunUser()
})

# ---------------------------------------------------------------------------
# declineReviewInvitation | ics1477
# Transition: Open -> Reviewing -> Declined
# ---------------------------------------------------------------------------
test_that("declineReviewInvitation declines a review|ics1477,ics2045", {
  stopifnot("No test folder" = exists("REVLC_FOLDER", envir = globalenv()))
  stopifnot("multi-user testbed required (hasConnectAs() must be TRUE)" = hasConnectAs())
  testFolder <- get("REVLC_FOLDER", envir = globalenv())
  testFile <- get("REVLC_FILE", envir = globalenv())
  test1Id <- get("REVLC_TEST1_ID", envir = globalenv())

  reviewName <- paste0("DeclineReview-", uniqueTag(6))
  reviewData <- createTestReview(reviewName, testFolder, testFile, test1Id)
  expect_false(is.null(reviewData))

  statusOk <- improveR::changeReviewStatus(reviewData, "Reviewing")
  expect_true(statusOk, info = "Could not transition review to Reviewing state")

  # Switch to test1 to decline
  improveRtestsupport::connectAs("test1")
  improveR::setEditable(TRUE)

  result <- improveR::declineReviewInvitation(reviewData, comment = "automated decline by test1")
  expect_true(result)
  cat("Declined review:", reviewData$resourceId, "\n")

  reconnectAsRunUser()
})

# ---------------------------------------------------------------------------
# changeReviewStatus | ccs27
# ---------------------------------------------------------------------------
test_that("changeReviewStatus Open to Reviewing|ccs27,ics2045", {
  stopifnot("No test folder" = exists("REVLC_FOLDER", envir = globalenv()))
  testFolder <- get("REVLC_FOLDER", envir = globalenv())
  testFile <- get("REVLC_FILE", envir = globalenv())
  test1Id <- get("REVLC_TEST1_ID", envir = globalenv())

  reviewName <- paste0("StatusReview1-", uniqueTag(6))
  reviewData <- createTestReview(reviewName, testFolder, testFile, test1Id)
  expect_false(is.null(reviewData))

  # Open -> Reviewing
  result <- improveR::changeReviewStatus(reviewData, "Reviewing")
  expect_true(result)
  cat("Changed review to Reviewing:", reviewData$resourceId, "\n")
})

test_that("changeReviewStatus Reviewing to Accepted via accept|ccs27,ics2045", {
  stopifnot("No test folder" = exists("REVLC_FOLDER", envir = globalenv()))
  stopifnot("multi-user testbed required (hasConnectAs() must be TRUE)" = hasConnectAs())
  testFolder <- get("REVLC_FOLDER", envir = globalenv())
  testFile <- get("REVLC_FILE", envir = globalenv())
  test1Id <- get("REVLC_TEST1_ID", envir = globalenv())

  reviewName <- paste0("StatusReview2-", uniqueTag(6))
  reviewData <- createTestReview(reviewName, testFolder, testFile, test1Id)
  expect_false(is.null(reviewData))

  # Open -> Reviewing
  result1 <- improveR::changeReviewStatus(reviewData, "Reviewing")
  expect_true(result1)

  # Verify state via getReviewById
  reviewInfo <- improveR::getReviewById(reviewData)
  expect_false(is.null(reviewInfo))
  cat("Review status after transition:", reviewInfo$status, "\n")

  # Accept as test1
  improveRtestsupport::connectAs("test1")
  improveR::setEditable(TRUE)
  result2 <- improveR::acceptReviewInvitation(reviewData, comment = "status test accept")
  expect_true(result2)
  cat("Accepted review via status flow:", reviewData$resourceId, "\n")

  reconnectAsRunUser()
})

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
test_that("cleanup review lifecycle test environment", {
  # Ensure we're admin
  if (hasConnectAs()) {
    tryCatch(reconnectAsRunUser(), error = function(e) NULL)
  }
  if (exists("REVLC_FOLDER", envir = globalenv())) {
    testFolder <- get("REVLC_FOLDER", envir = globalenv())
    tryCatch(improveR::delete(testFolder$resourceId), error = function(e) {
      cat("Cleanup warning:", e$message, "\n")
    })
    rm("REVLC_FOLDER", envir = globalenv())
  }
  if (exists("REVLC_FILE", envir = globalenv())) rm("REVLC_FILE", envir = globalenv())
  if (exists("REVLC_REVIEW", envir = globalenv())) rm("REVLC_REVIEW", envir = globalenv())
  if (exists("REVLC_TEST1_ID", envir = globalenv())) rm("REVLC_TEST1_ID", envir = globalenv())
  expect_true(TRUE)
})
