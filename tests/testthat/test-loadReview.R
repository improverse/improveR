# Test loadReview surface (ics1208)
# Spec ics1208 mandates loading reviewers, review entries, and review comments
# by resourceId/entityId/entityVersionId, with caching and POSIX dates.
# The implementation splits this into loadReviewers, loadReviewEntries,
# loadReviewComments, loadReviewEntryComments (and loadReviews for the list of
# all reviews).

Sys.setenv(TEST_NAME = "loadReview")

# Multi-user helpers — used by the comment-creation test which needs to
# accept-as-reviewer and then transition the review to "Reviewing" before
# createReviewComment passes its validateReviewState gate.
hasConnectAs <- function() {
  "improveRtestsupport" %in% loadedNamespaces() &&
    exists("connectAs", envir = asNamespace("improveRtestsupport"))
}

reconnectAsAdmin <- function() {
  tryCatch({
    improveRtestsupport::connectAs("admin")
    improveR::setEditable(TRUE)
  }, error = function(e) {
    improveR::clearConnectionData(includeRepoData = FALSE)
    improveR::improveConnect()
    improveR::setEditable(TRUE)
  })
}

setupLoadReview <- function() {
  Sys.setenv(TEST_NAME = "loadReview")
  improveR::improveConnect()
  improveR::setEditable(TRUE)

  basePath <- createFolderPath("loadReview")
  testFolder <- improveR::createFolder(
    targetIdent = basePath,
    folderName = paste0("lrv-", format(Sys.time(), "%Y%m%d%H%M%S")),
    comment = "loadReview test setup"
  )
  testFile <- improveR::createFile(
    targetIdent = testFolder$resourceId,
    fileName = "lrv-target.txt",
    comment = "target file for review"
  )

  # Find a reviewer (prefer test1, else any non-admin active user)
  allUsers <- improveR::users()
  reviewerId <- NULL
  if (!is.null(allUsers) && nrow(allUsers) > 0) {
    t1 <- which(allUsers$username == "test1")
    if (length(t1) > 0) {
      reviewerId <- allUsers$id[t1[1]]
    } else {
      nonAdmin <- allUsers[allUsers$username != "admin", , drop = FALSE]
      if (nrow(nonAdmin) > 0) reviewerId <- nonAdmin$id[1]
    }
  }

  reviewData <- NULL
  if (!is.null(reviewerId)) {
    reviewData <- tryCatch(
      improveR::createReview(
        name = paste0("LRV-", format(Sys.time(), "%H%M%S")),
        parentIdent = testFolder$path,
        comment = "loadReview test",
        templateId = NULL,
        resourceIds = list(testFile$resourceId),
        reviewerIds = list(reviewerId),
        dueDate = format(Sys.Date() + 30, "%Y-%m-%d")
      ),
      error = function(e) NULL
    )
  }

  list(
    testFolder = testFolder,
    testFile = testFile,
    reviewerId = reviewerId,
    reviewData = reviewData
  )
}

test_that("loadReviewers loads reviewers by resourceId with caching|ics1208", {
  ctx <- setupLoadReview()
  on.exit(tryCatch(improveR::delete(ctx$testFolder$resourceId),
                   error = function(e) NULL), add = TRUE)
  stopifnot("createReview failed (no reviewer available)" = !is.null(ctx$reviewData))

  reviewers <- improveR::loadReviewers(ctx$reviewData$resourceId)
  expect_false(is.null(reviewers))
  expect_true(is.data.frame(reviewers))
  expect_gte(nrow(reviewers), 1)
  expect_equal(unique(reviewers$resourceId), ctx$reviewData$resourceId)

  # Second call hits cache — identical result
  reviewers2 <- improveR::loadReviewers(ctx$reviewData$resourceId)
  expect_equal(reviewers, reviewers2)
})

test_that("loadReviewers accepts entityId and path identifiers|ics1208", {
  ctx <- setupLoadReview()
  on.exit(tryCatch(improveR::delete(ctx$testFolder$resourceId),
                   error = function(e) NULL), add = TRUE)
  stopifnot("createReview failed" = !is.null(ctx$reviewData))

  byEntity <- improveR::loadReviewers(ctx$reviewData$entityId)
  expect_false(is.null(byEntity))
  byPath <- improveR::loadReviewers(ctx$reviewData$path)
  expect_false(is.null(byPath))
  expect_equal(nrow(byEntity), nrow(byPath))
})

test_that("loadReviewEntries returns review entries|ics1208", {
  ctx <- setupLoadReview()
  on.exit(tryCatch(improveR::delete(ctx$testFolder$resourceId),
                   error = function(e) NULL), add = TRUE)
  stopifnot("createReview failed" = !is.null(ctx$reviewData))

  entries <- improveR::loadReviewEntries(ctx$reviewData$resourceId)
  expect_false(is.null(entries))
  expect_true(is.data.frame(entries))
  # Review was created with resourceIds=list(testFile) — at least one entry
  expect_gte(nrow(entries), 1)
})

test_that("loadReviewComments returns (possibly empty) comments|ics1208", {
  ctx <- setupLoadReview()
  on.exit(tryCatch(improveR::delete(ctx$testFolder$resourceId),
                   error = function(e) NULL), add = TRUE)
  stopifnot("createReview failed" = !is.null(ctx$reviewData))

  # createReviewComment requires reviewStatus == "Reviewing" (validateReviewState
  # enforces it). Transition the review out of the default state and verify the
  # comment was actually persisted before reading it back.
  stopifnot("multi-user testbed required (hasConnectAs() must be TRUE)" = hasConnectAs())
  improveRtestsupport::connectAs("test1")
  improveR::setEditable(TRUE)
  accepted <- improveR::acceptReviewInvitation(ctx$reviewData$resourceId,
                                               comment = "accept for loadReviewComments test")
  reconnectAsAdmin()
  stopifnot("acceptReviewInvitation returned FALSE" = isTRUE(accepted))

  improveR::changeReviewStatus(ctx$reviewData$resourceId, "Reviewing")
  improveR::refreshResource(ctx$reviewData$resourceId)

  created <- improveR::createReviewComment(ctx$reviewData$resourceId,
                                           resourceIdent = ctx$testFile$resourceId,
                                           comment = "lrv test comment",
                                           commentType = "GENERAL")
  stopifnot("createReviewComment returned NULL — comment was not created" = !is.null(created))

  comments <- improveR::loadReviewComments(ctx$reviewData$resourceId)
  expect_false(is.null(comments))
  expect_true(is.data.frame(comments))
  expect_gte(nrow(comments), 1)
})

test_that("loadReviews returns data frame or NULL (list of all reviews)|ics1208", {
  improveR::improveConnect()
  reviews <- improveR::loadReviews()
  expect_true(is.null(reviews) || is.data.frame(reviews))
})
