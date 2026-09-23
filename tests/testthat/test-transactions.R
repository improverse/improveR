# Test Transaction Functions
# Tests: getLatestRevision, createTransaction

test_that("setup transactions test", {
  tryCatch({
    improveR::improveConnect()
    improveR::setEditable(TRUE)
  }, error = function(e) {
    skip(paste("Server not available:", e$message))
  })
  expect_true(TRUE)
})

test_that("getLatestRevision returns a revision with an ID|ccs10", {
  rev <- improveR::getLatestRevision()
  requireServerCall(rev, "getLatestRevision")

  expect_true(is.list(rev))
  expect_false(is.null(rev$id))
  expect_true(nchar(rev$id) > 0)
})

test_that("createTransaction creates a new revision|ccs11", {
  revBefore <- improveR::getLatestRevision()
  requireServerCall(revBefore, "getLatestRevision")

  tx <- improveR::createTransaction(comment = "automated test transaction")
  requireServerCall(tx, "createTransaction")

  expect_false(is.null(tx$id))
  # The transaction should be a NEW revision, different from before
  expect_true(tx$id != revBefore$id,
              info = "Transaction should create a new revision ID")
  # And it should now be the latest
  revAfter <- improveR::getLatestRevision()
  expect_equal(revAfter$id, tx$id,
               info = "Latest revision should match the created transaction")
})

test_that("mutations create new revisions|ccs10", {
  revBefore <- improveR::getLatestRevision()
  requireServerCall(revBefore, "getLatestRevision")

  TEST_FOLDER <- improveR:::workflowFilesSetup()
  createFile(TEST_FOLDER, fileName = paste0("revtest_", uniqueTag(6), ".txt"))

  revAfter <- improveR::getLatestRevision()
  expect_true(revAfter$id != revBefore$id,
              info = "Creating a file should produce a new revision")
})
