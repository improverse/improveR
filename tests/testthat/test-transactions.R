# Test Transaction Functions
# Tests: getLatestRevision, createTransaction

ensureTestFolder <- function() {
  if (!exists("TEST_CONNECTED", envir = globalenv())) {
    improveR::improveConnect()
    improveR::setEditable(TRUE)
    assign("TEST_CONNECTED", TRUE, envir = globalenv())
  }
}

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------
test_that("setup transactions test environment", {
  improveR::improveConnect()
  improveR::setEditable(TRUE)
  assign("TEST_CONNECTED", TRUE, envir = globalenv())
  expect_true(TRUE)
})

# ---------------------------------------------------------------------------
# getLatestRevision | ccs10
# ---------------------------------------------------------------------------
test_that("getLatestRevision retrieves the latest revision|ccs10", {
  ensureTestFolder()
  result <- improveR::getLatestRevision()
  if (is.null(result)) {
    stop("getLatestRevision not supported on this server")
  }
  expect_true(is.list(result))
  expect_false(is.null(result$id))
  cat("Latest revision retrieved:", result$id, "\n")
})

# ---------------------------------------------------------------------------
# createTransaction | ccs11
# ---------------------------------------------------------------------------
test_that("createTransaction opens a new transaction|ccs11", {
  ensureTestFolder()
  result <- improveR::createTransaction(comment = "automated test transaction")
  if (is.null(result)) {
    stop("createTransaction not supported on this server")
  }
  expect_true(is.list(result))
  expect_false(is.null(result$id))
  cat("Created transaction:", result$id, "\n")
})

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
test_that("cleanup transactions test environment", {
  if (exists("TEST_CONNECTED", envir = globalenv())) rm("TEST_CONNECTED", envir = globalenv())
  expect_true(TRUE)
})
