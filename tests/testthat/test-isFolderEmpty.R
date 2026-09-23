# The condition that decides whether a test fixture has to build its folder
# content. It used to be written as
#     if (nrow(loadChildResources(path)$data[[1]]) == 0)
# which aborts with "argument is of length zero" on an empty folder - the one
# state it exists to detect (IMR-271). No server needed: the loader is injected.

test_that("an empty folder is recognised as empty|ics1085", {
  expect_true(improveR:::isFolderEmpty("/p", loader = function(p) list(data = list(NULL))))
  expect_true(improveR:::isFolderEmpty("/p", loader = function(p) list(data = list(data.frame()))))
  expect_true(improveR:::isFolderEmpty("/p", loader = function(p) list(data = list(list()))))
})

test_that("a folder with children is not empty|ics1085", {
  one <- data.frame(name = "a", stringsAsFactors = FALSE)
  two <- data.frame(name = c("a", "b"), stringsAsFactors = FALSE)
  expect_false(improveR:::isFolderEmpty("/p", loader = function(p) list(data = list(one))))
  expect_false(improveR:::isFolderEmpty("/p", loader = function(p) list(data = list(two))))
})

test_that("a loader that returns nothing at all counts as empty, not as an error|ics1085", {
  # loadChildResources() returns NULL on any non-2xx; the fixture must then
  # build its content rather than abort.
  expect_true(improveR:::isFolderEmpty("/p", loader = function(p) NULL))
  expect_true(improveR:::isFolderEmpty("/p", loader = function(p) list()))
})

test_that("a loader that raises does not propagate the error|ics1085", {
  expect_true(improveR:::isFolderEmpty("/p", loader = function(p) stop("server down")))
})
