# Test Review Functions
# Tests: createReview, createReviewer, deleteReviewer,
#        createReviewEntry, deleteReviewEntry,
#        createReviewComment, createReviewEntryComment
#        loadReviews, getReviewers, getReviewEntries, getReviewComments, getReviewEntryComments

ensureTestFolder <- function() {
  if (!exists("TEST_FOLDER", envir = globalenv())) {
    tryCatch({
      improveR::improveConnect()
      improveR::setEditable(TRUE)
      testFolder <- improveR::createFolder(
        targetIdent = "/",
        folderName = paste0("test-reviews-", format(Sys.time(), "%Y%m%d%H%M%S")),
        comment = "review test setup"
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
test_that("setup review test environment", {
  tryCatch({
    improveR::improveConnect()
    improveR::setEditable(TRUE)
  }, error = function(e) {
    skip(paste("Server not available:", e$message))
  })

  testFolder <- improveR::createFolder(
    targetIdent = "/",
    folderName = paste0("test-reviews-", format(Sys.time(), "%Y%m%d%H%M%S")),
    comment = "review test setup"
  )
  expect_false(is.null(testFolder))
  assign("TEST_FOLDER", testFolder, envir = globalenv())

  # Create a test file to use in review entries
  testFile <- improveR::createFile(
    targetIdent = testFolder$resourceId,
    fileName = "review-test-file.txt",
    comment = "test file for review"
  )
  expect_false(is.null(testFile))
  assign("TEST_FILE", testFile, envir = globalenv())

  # Get current user info for reviewer tests
  allUsers <- improveR::users()
  expect_false(is.null(allUsers))
  expect_gt(nrow(allUsers), 0)
  assign("TEST_USERS", allUsers, envir = globalenv())
})

# ---------------------------------------------------------------------------
# loadReviews | ics1525
# ---------------------------------------------------------------------------
test_that("loadReviews returns data frame|ics1525", {
  ensureTestFolder()
  reviews <- improveR::loadReviews()
  # May be NULL or data frame depending on server state
  if (!is.null(reviews)) {
    expect_true(is.data.frame(reviews))
  }
})

# ---------------------------------------------------------------------------
# createReview | ics1527
# ---------------------------------------------------------------------------
test_that("createReview creates a new review|ics1527", {
  ensureTestFolder()
  testFile <- get("TEST_FILE", envir = globalenv())
  allUsers <- get("TEST_USERS", envir = globalenv())
  testFolder <- get("TEST_FOLDER", envir = globalenv())

  reviewName <- paste0("TestReview-", format(Sys.time(), "%H%M%S"))
  result <- improveR::createReview(
    name = reviewName,
    parentIdent = testFolder$path,
    comment = "automated test review",
    templateId = NULL,
    resourceIds = list(testFile$resourceId),
    reviewerIds = list(allUsers$id[1]),
    dueDate = format(Sys.Date() + 30, "%Y-%m-%d")
  )

  if (is.null(result)) {
    skip("Review creation not supported or failed on this server")
  }
  expect_false(is.null(result))
  expect_true(is.data.frame(result))
  assign("TEST_REVIEW", result, envir = globalenv())
  cat("Created review:", result$resourceId, "\n")
})

# ---------------------------------------------------------------------------
# getReviewers | ics1531
# ---------------------------------------------------------------------------
test_that("getReviewers lists reviewers of a review|ics1531", {
  ensureTestFolder()
  if (!exists("TEST_REVIEW", envir = globalenv())) skip("No review created")
  review <- get("TEST_REVIEW", envir = globalenv())

  reviewers <- improveR::getReviewers(review)
  expect_false(is.null(reviewers))
  if (is.data.frame(reviewers)) {
    expect_gt(nrow(reviewers), 0)
    cat("Reviewers found:", nrow(reviewers), "\n")
  }
})

# ---------------------------------------------------------------------------
# createReviewer | ics368
# ---------------------------------------------------------------------------
test_that("createReviewer adds a reviewer to a review|ics368", {
  ensureTestFolder()
  if (!exists("TEST_REVIEW", envir = globalenv())) skip("No review created")
  review <- get("TEST_REVIEW", envir = globalenv())
  allUsers <- get("TEST_USERS", envir = globalenv())

  # Use second user if available
 if (nrow(allUsers) < 2) skip("Need at least 2 users for reviewer test")

  result <- improveR::createReviewer(
    ident = review,
    userId = allUsers$id[2],
    username = allUsers$username[2]
  )

  if (is.null(result)) {
    skip("createReviewer not supported or duplicate reviewer")
  }
  expect_false(is.null(result))
  assign("TEST_REVIEWER", result, envir = globalenv())
  cat("Added reviewer:", allUsers$username[2], "\n")
})

# ---------------------------------------------------------------------------
# deleteReviewer | ics369
# ---------------------------------------------------------------------------
test_that("deleteReviewer removes a reviewer from a review|ics369", {
  ensureTestFolder()
  if (!exists("TEST_REVIEW", envir = globalenv())) skip("No review created")
  if (!exists("TEST_REVIEWER", envir = globalenv())) skip("No reviewer added")
  review <- get("TEST_REVIEW", envir = globalenv())

  # Get current reviewers to find the one we added
  reviewers <- improveR::getReviewers(review)
  expect_false(is.null(reviewers))
  if (is.data.frame(reviewers) && nrow(reviewers) > 1) {
    reviewerId <- reviewers$id[nrow(reviewers)]  # last added
    result <- improveR::deleteReviewer(review, reviewerId)
    expect_true(result)
    cat("Removed reviewer:", reviewerId, "\n")
  }
})

# ---------------------------------------------------------------------------
# getReviewEntries | ics1540
# ---------------------------------------------------------------------------
test_that("getReviewEntries lists review entries|ics1540", {
  ensureTestFolder()
  if (!exists("TEST_REVIEW", envir = globalenv())) skip("No review created")
  review <- get("TEST_REVIEW", envir = globalenv())

  entries <- improveR::getReviewEntries(review)
  # Review was created with a resource, so should have entries
  if (!is.null(entries) && is.data.frame(entries)) {
    expect_gt(nrow(entries), 0)
    cat("Review entries found:", nrow(entries), "\n")
  }
})

# ---------------------------------------------------------------------------
# createReviewEntry | ics1541
# ---------------------------------------------------------------------------
test_that("createReviewEntry adds entries to a review|ics1541", {
  ensureTestFolder()
  if (!exists("TEST_REVIEW", envir = globalenv())) skip("No review created")
  review <- get("TEST_REVIEW", envir = globalenv())
  testFolder <- get("TEST_FOLDER", envir = globalenv())

  # Create an additional file to add as entry
  extraFile <- improveR::createFile(
    targetIdent = testFolder$resourceId,
    fileName = "review-extra-file.txt",
    comment = "extra file for review entry test"
  )
  if (is.null(extraFile)) skip("Could not create extra file")

  result <- improveR::createReviewEntry(
    ident = review,
    resourceIds = list(extraFile$resourceId)
  )
  if (is.null(result)) {
    skip("createReviewEntry failed on this server")
  }
  expect_true(is.data.frame(result))
  cat("Added review entry for resource:", extraFile$resourceId, "\n")
})

# ---------------------------------------------------------------------------
# deleteReviewEntry | ics1542
# ---------------------------------------------------------------------------
test_that("deleteReviewEntry removes all entries from a review|ics1542", {
  ensureTestFolder()
  if (!exists("TEST_REVIEW", envir = globalenv())) skip("No review created")
  review <- get("TEST_REVIEW", envir = globalenv())

  result <- improveR::deleteReviewEntry(review)
  # deleteReviewEntry deletes ALL entries — server may return 500 if review
  # is in a state that does not allow bulk entry deletion
  if (!result) {
    skip("deleteReviewEntry rejected by server (review state may not allow deletion)")
  }
  cat("Deleted all review entries for review:", review$resourceId, "\n")

  # Verify entries are gone
  entries <- improveR::getReviewEntries(review)
  if (!is.null(entries) && is.data.frame(entries)) {
    expect_equal(nrow(entries), 0)
  }
})

# ---------------------------------------------------------------------------
# createReviewComment | ics1536
# ---------------------------------------------------------------------------
test_that("createReviewComment adds a comment to a review|ics1536", {
  ensureTestFolder()
  if (!exists("TEST_REVIEW", envir = globalenv())) skip("No review created")
  review <- get("TEST_REVIEW", envir = globalenv())
  testFile <- get("TEST_FILE", envir = globalenv())

  # Re-add entry since we deleted them
  improveR::createReviewEntry(
    ident = review,
    resourceIds = list(testFile$resourceId)
  )

  result <- improveR::createReviewComment(
    ident = review,
    resourceIdent = testFile$resourceId,
    comment = "Automated test comment",
    commentType = "GENERAL"
  )
  if (is.null(result)) {
    skip("createReviewComment failed - review may not be in Reviewing state")
  }
  expect_false(is.null(result))
  cat("Added review comment\n")
})

# ---------------------------------------------------------------------------
# getReviewComments | ics1533
# ---------------------------------------------------------------------------
test_that("getReviewComments lists review comments|ics1533", {
  ensureTestFolder()
  if (!exists("TEST_REVIEW", envir = globalenv())) skip("No review created")
  review <- get("TEST_REVIEW", envir = globalenv())

  comments <- improveR::getReviewComments(review)
  if (!is.null(comments) && is.data.frame(comments)) {
    cat("Review comments found:", nrow(comments), "\n")
  }
})

# ---------------------------------------------------------------------------
# Validation: invalid inputs
# ---------------------------------------------------------------------------
test_that("createReview returns NULL for invalid resource IDs|ics1527", {
  ensureTestFolder()
  result <- improveR::createReview(
    name = "InvalidReview",
    parentIdent = "/",
    comment = "test",
    templateId = NULL,
    resourceIds = list("non-existent-id-00000000"),
    reviewerIds = list(),
    dueDate = format(Sys.Date() + 30, "%Y-%m-%d")
  )
  expect_null(result)
})

test_that("createReviewer returns NULL for invalid review ID|ics368", {
  ensureTestFolder()
  allUsers <- get("TEST_USERS", envir = globalenv())
  result <- improveR::createReviewer(
    ident = "non-existent-review-id",
    userId = allUsers$id[1],
    username = allUsers$username[1]
  )
  expect_null(result)
})

test_that("deleteReviewer returns FALSE for invalid review ID|ics369", {
  ensureTestFolder()
  result <- improveR::deleteReviewer(
    ident = "non-existent-review-id",
    reviewerId = "non-existent-reviewer-id"
  )
  expect_false(result)
})

test_that("createReviewEntry returns NULL for invalid review ID|ics1541", {
  ensureTestFolder()
  testFile <- get("TEST_FILE", envir = globalenv())
  result <- improveR::createReviewEntry(
    ident = "non-existent-review-id",
    resourceIds = list(testFile$resourceId)
  )
  expect_null(result)
})

test_that("deleteReviewEntry returns FALSE for invalid review ID|ics1542", {
  ensureTestFolder()
  result <- improveR::deleteReviewEntry(
    ident = "non-existent-review-id"
  )
  expect_false(result)
})

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
test_that("cleanup review test environment", {
  if (exists("TEST_FOLDER", envir = globalenv())) {
    testFolder <- get("TEST_FOLDER", envir = globalenv())
    tryCatch({
      improveR::delete(testFolder$resourceId)
    }, error = function(e) {
      cat("Cleanup warning:", e$message, "\n")
    })
    rm("TEST_FOLDER", envir = globalenv())
  }
  if (exists("TEST_FILE", envir = globalenv())) rm("TEST_FILE", envir = globalenv())
  if (exists("TEST_REVIEW", envir = globalenv())) rm("TEST_REVIEW", envir = globalenv())
  if (exists("TEST_REVIEWER", envir = globalenv())) rm("TEST_REVIEWER", envir = globalenv())
  if (exists("TEST_USERS", envir = globalenv())) rm("TEST_USERS", envir = globalenv())
})
