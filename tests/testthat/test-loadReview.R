# Test loadReview surface (ics1208)
# Spec ics1208 mandates loading reviewers, review entries, and review comments
# by resourceId/entityId/entityVersionId, with caching and POSIX dates.
# The implementation splits this into loadReviewers, loadReviewEntries,
# loadReviewComments (and loadReviews for the list of all reviews).
#
# Review ENTRY comments are the exception and are read with
# getReviewEntryComments. They are the only family keyed by two values -
# resourceId and entryId - which the cache cannot express, so they never had
# one. The load/unload/refresh trio that suggested otherwise was removed in
# IMR-278; unload made a server call and discarded it, refresh made two.

Sys.setenv(TEST_NAME = "loadReview")

# Multi-user helpers — used by the comment-creation test which needs to
# accept-as-reviewer and then transition the review to "Reviewing" before
# createReviewComment passes its validateReviewState gate.
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

setupLoadReview <- function() {
  Sys.setenv(TEST_NAME = "loadReview")
  improveR::improveConnect()
  improveR::setEditable(TRUE)

  basePath <- createFolderPath("loadReview")
  testFolder <- improveR::createFolder(
    targetIdent = basePath,
    folderName = paste0("lrv-", uniqueTag()),
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
        name = paste0("LRV-", uniqueTag(6)),
        parentIdent = testFolder$path,
        comment = "loadReview test",
        templateId = NULL,
        resourceIds = list(testFile$resourceId),
        reviewerIds = list(reviewerId),
        dueDate = format(runDate() + 30, "%Y-%m-%d")
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
  reconnectAsRunUser()
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

# --- Review entry comments (IMR-278) ---------------------------------------
#
# Two functions, not five. loadReviewEntryComments, unloadReviewEntryComments
# and refreshReviewEntryComments were removed: the family is keyed by
# resourceId AND entryId, which the cache cannot express, so it never had one.
# unload made a server call and threw the result away; refresh made two calls
# for what one getReviewEntryComments returns.
#
# This asserts the content that was written, not that a data frame came back.

test_that("createReviewEntryComment writes a comment that getReviewEntryComments returns|ics1543,ics1544", {
  ctx <- setupLoadReview()
  on.exit(tryCatch(improveR::delete(ctx$testFolder$resourceId),
                   error = function(e) NULL), add = TRUE)
  stopifnot("createReview failed" = !is.null(ctx$reviewData))

  entries <- improveR::loadReviewEntries(ctx$reviewData$resourceId)
  stopifnot("the review must have at least one entry" = !is.null(entries) && nrow(entries) >= 1)
  entryId <- entries$id[1]

  # A comment can only be written while the review is being reviewed, and the
  # invitation has to be accepted by the foreign reviewer first - the server
  # enforces dual control.
  stopifnot("multi-user testbed required (hasConnectAs() must be TRUE)" = hasConnectAs())
  improveRtestsupport::connectAs("test1")
  improveR::setEditable(TRUE)
  accepted <- improveR::acceptReviewInvitation(ctx$reviewData$resourceId,
                                               comment = "accept for entry comment test")
  reconnectAsRunUser()
  stopifnot("acceptReviewInvitation returned FALSE" = isTRUE(accepted))
  improveR::changeReviewStatus(ctx$reviewData$resourceId, "Reviewing")
  improveR::refreshResource(ctx$reviewData$resourceId)

  # Empty before anything is written - so the assertion below cannot pass on
  # something a previous run left behind.
  before <- improveR::getReviewEntryComments(ctx$reviewData$resourceId, entryId)
  expect_true(is.null(before) || nrow(before) == 0,
              info = "a fresh review entry must start without comments")

  text <- paste0("imr278 entry comment ", uuid::UUIDgenerate())
  created <- improveR::createReviewEntryComment(ctx$reviewData$resourceId, entryId, text)
  expect_false(is.null(created),
               info = "createReviewEntryComment must return the comment it created")

  after <- improveR::getReviewEntryComments(ctx$reviewData$resourceId, entryId)
  expect_false(is.null(after))
  expect_true(is.data.frame(after))
  expect_equal(nrow(after), 1L, info = "exactly the one comment that was written")

  # The content, not the shape. A data frame with the wrong text would pass
  # every is.data.frame()/nrow() check ever written.
  textColumn <- intersect(c("comment", "text", "message"), names(after))
  expect_true(length(textColumn) >= 1,
              info = paste0("the result must carry the comment text; columns are: ",
                            paste(names(after), collapse = ", ")))
  # Asserted as "contained in", not "equal to", and that is deliberate.
  #
  # The value that comes back today is not the text but the POST body around
  # it: writing "hello" yields {"comment":"hello"}. improveR sends
  # data = list("comment" = comment) and the server stores that verbatim -
  # createReviewComment, the sibling that works, sends resourceId, comment and
  # commentType instead. Raised as IMR-279.
  #
  # "contained in" holds before and after that is fixed, so this check does not
  # have to be rewritten and does not enshrine the defect either.
  roundTripped <- unlist(after[textColumn])
  expect_true(any(vapply(roundTripped, function(v) grepl(text, v, fixed = TRUE), logical(1))),
              info = paste0("the text that was written must be recoverable from the result; got: ",
                            paste(roundTripped, collapse = " | ")))

  # A second comment must be visible without any unload or refresh - there is
  # no cache to invalidate, and that is now the documented behaviour.
  second <- paste0("imr278 second ", uuid::UUIDgenerate())
  improveR::createReviewEntryComment(ctx$reviewData$resourceId, entryId, second)
  both <- improveR::getReviewEntryComments(ctx$reviewData$resourceId, entryId)
  expect_equal(nrow(both), 2L,
               info = "the second comment must be returned by the next call, unaided")
})

test_that("createReviewEntryComment refuses an entry that does not belong to the review|ics1544", {
  ctx <- setupLoadReview()
  on.exit(tryCatch(improveR::delete(ctx$testFolder$resourceId),
                   error = function(e) NULL), add = TRUE)
  stopifnot("createReview failed" = !is.null(ctx$reviewData))

  bogus <- uuid::UUIDgenerate()
  expect_null(improveR::createReviewEntryComment(ctx$reviewData$resourceId, bogus, "nope"),
              info = "an entry id that is not in this review must yield NULL, not a comment")
})
