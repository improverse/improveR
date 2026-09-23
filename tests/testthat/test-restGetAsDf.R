# restGetAsDf: a JSON object is one thing, not a collection of things (IMR-294)
#
# No server. authenticatedREST() and httr::content() are mocked, which is the
# only way to hand restGetAsDf() a chosen response shape - and the shape is the
# whole defect.
#
# What went wrong: restGetAsDf() passed every parsed response to
# mergeListToDataframe(), which iterates its argument and makes each element a
# row. For a JSON array that is right. For a JSON object it iterates the
# object's FIELDS, so a 14-field response became 14 rows, each a single-column
# frame with a different column name, merged into 14 rows and 0 columns.
#
# loadFavorites() returned exactly that: a data frame that is not empty, has no
# columns, and cannot be used for anything. The function for listing favourites
# could not list them - and because nrow() was 14 the "no rows" guard below it
# never fired.

fakeResponse <- function() structure(list(status_code = 200L), class = "response")

# httr::content is not in improveR's namespace, so it is mocked where it is
# looked up: as httr's own binding.
#
# repoPrefix() is mocked because mergeListToDataframe() prefixes any entityId it
# finds, and repoPrefix() reads the connection configuration. These tests have
# no connection, and the prefix is not what they are about.
callWith <- function(parsed, ...) {
  testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      improveR:::restGetAsDf("/whatever", ...),
      content = function(...) parsed,
      .package = "httr"
    ),
    authenticatedREST = function(...) fakeResponse(),
    repoPrefix        = function(...) "testrepo:",
    .package = "improveR"
  )
}

test_that("a single JSON object becomes one row, not one row per field|IMR-294", {
  # The exact shape GET /api/v1/favorites returns, taken from a recording of
  # test-favorites.R against repository 4315 - fourteen fields, which is where
  # the fourteen rows in the report came from.
  obj <- list(
    resourceId                = "C4FB0DEE901345F885BE41CA64858B18",
    resourceVersionId         = "A1B2C3D4E5F6478899AABBCCDDEEFF00",
    nodeType                  = "Folder",
    name                      = "Favorites",
    deleted                   = FALSE,
    entityId                  = "FO-170099",
    entityVersionId           = "FOV-170099",
    fullEntityId              = "testrepo:FO-170099",
    fullEntityVersionId       = "testrepo:FOV-170099",
    fileSize                  = 0L,
    hasChildren               = TRUE,
    hasChildrenIncludingFiles = TRUE,
    outdatedLink              = FALSE,
    workingFile               = FALSE
  )
  expect_length(obj, 14L)   # the 14 that became 14 rows

  df <- callWith(obj)

  expect_false(is.null(df))
  expect_equal(nrow(df), 1L)
  expect_true(ncol(df) >= 14L)
  expect_true("name" %in% names(df))
  expect_equal(df$name, "Favorites")
})

test_that("a JSON array of objects still becomes one row per element|IMR-294", {
  arr <- list(
    list(resourceId = "A", name = "one"),
    list(resourceId = "B", name = "two"),
    list(resourceId = "C", name = "three")
  )
  df <- callWith(arr)

  expect_equal(nrow(df), 3L)
  expect_equal(df$name, c("one", "two", "three"))
})

test_that("a one-element array is still an array, not an object|IMR-294", {
  df <- callWith(list(list(resourceId = "A", name = "only")))
  expect_equal(nrow(df), 1L)
  expect_equal(df$name, "only")
})

test_that("an empty response yields NULL rather than a frame with no columns|IMR-294", {
  expect_null(callWith(list()))
})

test_that("elementsKey is applied before the object/array decision|IMR-294", {
  wrapped <- list(elements = list(list(name = "a"), list(name = "b")))
  df <- callWith(wrapped, elementsKey = "elements")
  expect_equal(nrow(df), 2L)
  expect_equal(df$name, c("a", "b"))
})

test_that("an object under elementsKey becomes one row|IMR-294", {
  wrapped <- list(elements = list(name = "only", id = "X"))
  df <- callWith(wrapped, elementsKey = "elements")
  expect_equal(nrow(df), 1L)
  expect_equal(df$name, "only")
})

test_that("a refused call still returns NULL|IMR-294", {
  # authenticatedREST answers every non-2xx with NULL; restGetAsDf must not
  # try to read content out of it.
  out <- testthat::with_mocked_bindings(
    improveR:::restGetAsDf("/whatever"),
    authenticatedREST = function(...) NULL,
    repoPrefix        = function(...) "testrepo:",
    .package = "improveR"
  )
  expect_null(out)
})
