# Pure helper tests: no server, no connection, milliseconds.
# Recovered from _test-loadResourceVersionFromServer.R, where both checks sat
# commented out and had never run (IMR-263). Both helpers are internal to
# R/loadResourceVersionFromServer.r and are covered by no other test file.
# Qualified with improveR::: so the checks run against the installed package.

test_that("extractEntityId reduces an entityVersionId to its entityId|ics1084", {
  expect_equal(improveR:::extractEntityId("robert_oracle-1:ST-63657-1"),
               "robert_oracle-1:ST-63657")
})

test_that("cutPrefix removes the repository prefix from an entityVersionId|ics1084", {
  expect_equal(unlist(improveR:::cutPrefix("robert_oracle-1:ST-63657-1")),
               "ST-63657-1")
})
