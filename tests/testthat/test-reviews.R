# Test Review Functions
# Tests actual review behavior — verifying state changes on the server,
# not just return types.

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

getTest1UserId <- function(allUsers) {
  idx <- which(allUsers$username == "test1")
  if (length(idx) == 0) return(NULL)
  allUsers$id[idx[1]]
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

  basePath <- createFolderPath("reviews")
  testFolder <- improveR::createFolder(
    targetIdent = basePath,
    folderName = paste0("test-reviews-", uniqueTag()),
    comment = "review test setup"
  )
  expect_false(is.null(testFolder))
  assign("REV_FOLDER", testFolder, envir = globalenv())

  testFile <- improveR::createFile(
    targetIdent = testFolder$resourceId,
    fileName = "review-test-file.txt",
    comment = "test file for review"
  )
  expect_false(is.null(testFile))
  assign("REV_FILE", testFile, envir = globalenv())

  allUsers <- improveR::users()
  expect_false(is.null(allUsers))
  expect_gt(nrow(allUsers), 0)
  assign("REV_USERS", allUsers, envir = globalenv())

  test1Id <- getTest1UserId(allUsers)
  assign("REV_TEST1_ID", test1Id, envir = globalenv())
})

# ---------------------------------------------------------------------------
# createReview | ics1527
# Verify: review can be loaded back and has correct name and entries
# ---------------------------------------------------------------------------
test_that("createReview creates a review with correct name and entry|ics1527,ics2045", {
  stopifnot("No test folder" = exists("REV_FOLDER", envir = globalenv()))
  testFile <- get("REV_FILE", envir = globalenv())
  testFolder <- get("REV_FOLDER", envir = globalenv())
  test1Id <- get("REV_TEST1_ID", envir = globalenv())
  reviewerId <- if (!is.null(test1Id)) test1Id else get("REV_USERS", envir = globalenv())$id[1]

  reviewName <- paste0("TestReview-", uniqueTag(6))
  result <- improveR::createReview(
    name = reviewName,
    parentIdent = testFolder$path,
    comment = "automated test review",
    templateId = NULL,
    resourceIds = list(testFile$resourceId),
    reviewerIds = list(reviewerId),
    dueDate = format(runDate() + 30, "%Y-%m-%d")
  )

  requireServerCall(result, "createReview")

  # Verify: load review back by ID and check name
  loaded <- improveR::getReviewById(result)
  expect_false(is.null(loaded))
  expect_equal(loaded$name, reviewName,
               info = "Loaded review name should match what was created")

  # Verify: review has the file as an entry
  entries <- improveR::getReviewEntries(result)
  expect_true(is.data.frame(entries))
  expect_true(testFile$resourceId %in% entries$resourceId,
              info = "Review should contain the submitted file as an entry")

  assign("REV_REVIEW", result, envir = globalenv())
})

# ---------------------------------------------------------------------------
# createReviewer | ics368
# Verify: reviewer appears in getReviewers after being added
# ---------------------------------------------------------------------------
test_that("createReviewer adds a reviewer visible in getReviewers|ics368,ics2045", {
  stopifnot("No review created" = exists("REV_REVIEW", envir = globalenv()))
  review <- get("REV_REVIEW", envir = globalenv())
  allUsers <- get("REV_USERS", envir = globalenv())

  if (nrow(allUsers) < 2) skip("Need at least 2 users")

  test1Id <- get("REV_TEST1_ID", envir = globalenv())
  otherIdx <- which(allUsers$id != test1Id & allUsers$username != "admin")
  if (length(otherIdx) == 0) otherIdx <- which(allUsers$id != test1Id)
  skip_if(length(otherIdx) == 0, "No second user available")

  reviewersBefore <- improveR::getReviewers(review)
  countBefore <- if (is.data.frame(reviewersBefore)) nrow(reviewersBefore) else 0

  result <- improveR::createReviewer(
    ident = review,
    userId = allUsers$id[otherIdx[1]],
    username = allUsers$username[otherIdx[1]]
  )
  requireServerCall(result, "createReviewer")

  # Verify: reviewer count increased
  reviewersAfter <- improveR::getReviewers(review)
  expect_true(is.data.frame(reviewersAfter))
  expect_equal(nrow(reviewersAfter), countBefore + 1,
               info = "Reviewer count should increase by 1")

  assign("REV_REVIEWER", result, envir = globalenv())
})

# ---------------------------------------------------------------------------
# deleteReviewer | ics369
# Verify: reviewer disappears from getReviewers after deletion
# ---------------------------------------------------------------------------
test_that("deleteReviewer removes reviewer from getReviewers|ics369,ics2045", {
  stopifnot("No review created" = exists("REV_REVIEW", envir = globalenv()))
  stopifnot("No reviewer added" = exists("REV_REVIEWER", envir = globalenv()))
  stopifnot("No REV_TEST1_ID for safe deletion" = exists("REV_TEST1_ID", envir = globalenv()))
  review <- get("REV_REVIEW", envir = globalenv())
  test1Id <- get("REV_TEST1_ID", envir = globalenv())

  reviewersBefore <- improveR::getReviewers(review)
  expect_true(is.data.frame(reviewersBefore))
  stopifnot("Need at least 2 reviewers to test delete" = nrow(reviewersBefore) >= 2)

  countBefore <- nrow(reviewersBefore)

  # Pick a reviewer that is NOT test1. The downstream test_thats
  # (createReviewComment, approve/reset/reject workflow) connect as test1
  # and call acceptReviewInvitation; if we delete test1 here those tests
  # fail with "acceptReviewInvitation returned FALSE" because the server
  # rejects accept calls from a user who is no longer a reviewer. The
  # previous logic targeted reviewersBefore$id[nrow(...)] — the last row
  # — which is unstable: the server does not guarantee reviewer ordering,
  # so the same test ran green in some suites and red in others depending
  # on which row came back last. Find a non-test1 reviewer explicitly.
  # The server reports the user id under either "user.id" or "userId"
  # depending on REST flattening — mirror the dispatch in
  # validateDuplicateReviewer() so this test works on both shapes.
  userIdCol <- if ("user.id" %in% colnames(reviewersBefore)) "user.id"
               else if ("userId" %in% colnames(reviewersBefore)) "userId"
               else NULL
  stopifnot("getReviewers result has no user.id / userId column" = !is.null(userIdCol))
  candidates <- reviewersBefore[reviewersBefore[[userIdCol]] != test1Id, , drop = FALSE]
  stopifnot("No non-test1 reviewer available to delete safely" = nrow(candidates) >= 1)
  reviewerToDelete <- candidates$id[1]

  result <- improveR::deleteReviewer(review, reviewerToDelete)
  expect_true(result)

  # Verify: reviewer count decreased
  reviewersAfter <- improveR::getReviewers(review)
  expect_equal(nrow(reviewersAfter), countBefore - 1,
               info = "Reviewer count should decrease by 1")
  # Verify: deleted reviewer is gone, test1 still in the list
  expect_false(reviewerToDelete %in% reviewersAfter$id,
               info = "Deleted reviewer should not appear in list")
  expect_true(test1Id %in% reviewersAfter[[userIdCol]],
              info = "test1 must remain a reviewer for downstream tests")
})

# ---------------------------------------------------------------------------
# createReviewEntry | ics1541
# Verify: entry count increases and new entry contains the file
# ---------------------------------------------------------------------------
test_that("createReviewEntry adds entry visible in getReviewEntries|ics1541,ics2045", {
  stopifnot("No review created" = exists("REV_REVIEW", envir = globalenv()))
  review <- get("REV_REVIEW", envir = globalenv())
  testFolder <- get("REV_FOLDER", envir = globalenv())

  entriesBefore <- improveR::getReviewEntries(review)
  countBefore <- if (is.data.frame(entriesBefore)) nrow(entriesBefore) else 0

  extraFile <- improveR::createFile(
    targetIdent = testFolder$resourceId,
    fileName = paste0("entry-test-", uniqueTag(6), ".txt"),
    comment = "file for entry test"
  )
  expect_false(is.null(extraFile))

  improveR::createReviewEntry(
    ident = review,
    resourceIds = list(extraFile$resourceId)
  )

  # Verify: entry count increased
  entriesAfter <- improveR::getReviewEntries(review)
  expect_true(is.data.frame(entriesAfter))
  expect_equal(nrow(entriesAfter), countBefore + 1,
               info = "Entry count should increase by 1")
  # Verify: the new file is in the entries
  expect_true(extraFile$resourceId %in% entriesAfter$resourceId,
              info = "New file should appear in review entries")
})

# ---------------------------------------------------------------------------
# deleteReviewEntry | ics1542
# Verify: selective deletion — one entry removed, other remains
# ---------------------------------------------------------------------------
test_that("deleteReviewEntry removes specific entry, keeps others|ics1542,ics2045", {
  stopifnot("No review created" = exists("REV_REVIEW", envir = globalenv()))
  review <- get("REV_REVIEW", envir = globalenv())
  testFolder <- get("REV_FOLDER", envir = globalenv())

  fileA <- improveR::createFile(
    targetIdent = testFolder$resourceId,
    fileName = paste0("del-A-", uniqueTag(6), ".txt"),
    comment = "delete test A"
  )
  fileB <- improveR::createFile(
    targetIdent = testFolder$resourceId,
    fileName = paste0("del-B-", uniqueTag(6), ".txt"),
    comment = "delete test B"
  )
  improveR::createReviewEntry(
    ident = review,
    resourceIds = list(fileA$resourceId, fileB$resourceId)
  )

  entriesBefore <- improveR::getReviewEntries(review)
  countBefore <- nrow(entriesBefore)

  entryIdA <- entriesBefore$id[entriesBefore$resourceId == fileA$resourceId]
  expect_true(length(entryIdA) == 1)

  result <- improveR::deleteReviewEntry(review, reviewEntryIds = list(entryIdA))
  expect_true(result)

  entriesAfter <- improveR::getReviewEntries(review)
  expect_equal(nrow(entriesAfter), countBefore - 1,
               info = "Entry count should decrease by 1")
  expect_false(fileA$resourceId %in% entriesAfter$resourceId,
               info = "Deleted entry's file should be gone")
  expect_true(fileB$resourceId %in% entriesAfter$resourceId,
              info = "Other entry's file should remain")
})

# ---------------------------------------------------------------------------
# createReviewComment | ics1536
# Verify: comment appears in getReviewComments
# ---------------------------------------------------------------------------
test_that("createReviewComment adds comment visible in getReviewComments|ics1536,ics2045", {
  stopifnot("No review created" = exists("REV_REVIEW", envir = globalenv()))
  stopifnot("multi-user testbed required (hasConnectAs() must be TRUE)" = hasConnectAs())
  review <- get("REV_REVIEW", envir = globalenv())
  testFile <- get("REV_FILE", envir = globalenv())

  # Reviewer must accept invitation first (while review is in Planning)
  improveRtestsupport::connectAs("test1")
  improveR::setEditable(TRUE)
  accepted <- improveR::acceptReviewInvitation(review, comment = "accepting for comment test")
  reconnectAsRunUser()
  stopifnot("acceptReviewInvitation returned FALSE — comment-test prerequisite failed" = isTRUE(accepted))

  improveR::changeReviewStatus(review, "Reviewing")
  improveR::refreshResource(review$resourceId)

  # Ensure the file is an entry
  improveR::createReviewEntry(ident = review, resourceIds = list(testFile$resourceId))

  improveRtestsupport::connectAs("test1")
  improveR::setEditable(TRUE)

  commentText <- paste0("Test comment ", uniqueTag(6))
  result <- improveR::createReviewComment(
    ident = review,
    resourceIdent = testFile$resourceId,
    comment = commentText,
    commentType = "GENERAL"
  )

  requireServerCall(result, "createReviewComment", cleanup = function() reconnectAsRunUser())

  # Verify: comment appears in list
  comments <- improveR::getReviewComments(review)
  expect_true(is.data.frame(comments), info = "Should have comments after creating one")
  expect_true(any(grepl(commentText, comments$comment, fixed = TRUE)),
              info = "Created comment text should appear in getReviewComments")

  reconnectAsRunUser()
})

# ---------------------------------------------------------------------------
# approveReviewEntries | ics1545
# Verify: entry status changes after approval
# ---------------------------------------------------------------------------
test_that("approve, reset, reject entry workflow|ics1545,ics1547,ics1546", {
  stopifnot("No review created" = exists("REV_REVIEW", envir = globalenv()))
  stopifnot("multi-user testbed required (hasConnectAs() must be TRUE)" = hasConnectAs())
  review <- get("REV_REVIEW", envir = globalenv())

  # Reviewer should already have accepted invitation from the comment test.
  # Ensure we're in Reviewing state.
  review <- improveR::refreshResource(review$resourceId)
  improveR::changeReviewStatus(review, "Reviewing")
  improveR::refreshResource(review$resourceId)

  entries <- improveR::getReviewEntries(review)
  skip_if(is.null(entries) || nrow(entries) < 2,
          "Need at least 2 entries for approve/reset/reject workflow")

  # Use only the FIRST entry for approve/reset/reject so the review stays in Reviewing
  testEntryId <- entries$id[1]

  improveRtestsupport::connectAs("test1")
  improveR::setEditable(TRUE)

  # 1. Approve one entry
  result <- improveR::approveReviewEntries(
    ident = review,
    reviewEntryIds = list(testEntryId),
    comment = "approve one entry"
  )
  expect_true(result, info = "approveReviewEntries should succeed")

  entriesAfter <- improveR::getReviewEntries(review)
  approvedEntry <- entriesAfter[entriesAfter$id == testEntryId, ]
  expect_equal(approvedEntry$status, "Approved",
               info = "Approved entry should have Approved status")

  # 2. Reset that entry
  result <- improveR::resetReviewEntries(
    ident = review,
    reviewEntryIds = list(testEntryId),
    comment = "reset entry"
  )
  expect_true(result, info = "resetReviewEntries should succeed")

  entriesAfter <- improveR::getReviewEntries(review)
  resetEntry <- entriesAfter[entriesAfter$id == testEntryId, ]
  expect_true(resetEntry$status != "Approved",
              info = "Entry should not be Approved after reset")

  # 3. Reject that entry
  result <- improveR::rejectReviewEntries(
    ident = review,
    reviewEntryIds = list(testEntryId),
    comment = "reject entry"
  )
  expect_true(result, info = "rejectReviewEntries should succeed")

  entriesAfter <- improveR::getReviewEntries(review)
  rejectedEntry <- entriesAfter[entriesAfter$id == testEntryId, ]
  expect_equal(rejectedEntry$status, "Rejected",
               info = "Rejected entry should have DECLINED status")

  reconnectAsRunUser()
})

# ---------------------------------------------------------------------------
# Validation: invalid inputs
# ---------------------------------------------------------------------------
test_that("createReview returns NULL for invalid resource IDs|ics1527,ics2045", {
  result <- improveR::createReview(
    name = "InvalidReview",
    parentIdent = "/",
    comment = "test",
    templateId = NULL,
    resourceIds = list("non-existent-id-00000000"),
    reviewerIds = list(),
    dueDate = format(runDate() + 30, "%Y-%m-%d")
  )
  expect_null(result)
})

test_that("createReviewer returns NULL for invalid review ID|ics368,ics2045", {
  allUsers <- get("REV_USERS", envir = globalenv())
  result <- improveR::createReviewer(
    ident = "non-existent-review-id",
    userId = allUsers$id[1],
    username = allUsers$username[1]
  )
  expect_null(result)
})

test_that("deleteReviewer returns FALSE for invalid review ID|ics369,ics2045", {
  result <- improveR::deleteReviewer(
    ident = "non-existent-review-id",
    reviewerId = "non-existent-reviewer-id"
  )
  expect_false(result)
})

test_that("createReviewEntry returns NULL for invalid review ID|ics1541,ics2045", {
  testFile <- get("REV_FILE", envir = globalenv())
  result <- improveR::createReviewEntry(
    ident = "non-existent-review-id",
    resourceIds = list(testFile$resourceId)
  )
  expect_null(result)
})

test_that("deleteReviewEntry returns FALSE for invalid review ID|ics1542,ics2045", {
  result <- improveR::deleteReviewEntry(
    ident = "non-existent-review-id",
    reviewEntryIds = list("non-existent-entry-id")
  )
  expect_false(result)
})

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
test_that("cleanup review test environment", {
  if (exists("REV_FOLDER", envir = globalenv())) {
    testFolder <- get("REV_FOLDER", envir = globalenv())
    tryCatch(improveR::delete(testFolder$resourceId), error = function(e) {
      cat("Cleanup warning:", e$message, "\n")
    })
    rm("REV_FOLDER", envir = globalenv())
  }
  if (exists("REV_FILE", envir = globalenv())) rm("REV_FILE", envir = globalenv())
  if (exists("REV_REVIEW", envir = globalenv())) rm("REV_REVIEW", envir = globalenv())
  if (exists("REV_REVIEWER", envir = globalenv())) rm("REV_REVIEWER", envir = globalenv())
  if (exists("REV_USERS", envir = globalenv())) rm("REV_USERS", envir = globalenv())
  if (exists("REV_TEST1_ID", envir = globalenv())) rm("REV_TEST1_ID", envir = globalenv())
  expect_true(TRUE)
})
