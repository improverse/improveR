#REMOVE
test_that("Does fn addElementToCache work?", {

  # addElementToCache("new_element", "new_value")
  addElementToCache()

  expect_true(exists("new_element", envir = cacheEnv))
})