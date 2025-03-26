# TODO
httptest::with_mock_dir("loadResourceFromServerWorks", {

  test_that("loadResourceFromServerWorks", {
   
    improveConnect()

    loadResourceFromServerResult <- loadResourceFromServer(resourceId = "robert_oracle-1:ST-63657")
    testthat::expect_s3_class(loadResourceFromServerResult, "data.frame")
    testthat::expect_true(nrow(loadResourceFromServerResult)==1)

  })
})








test_that("Does cacheEnv exist", {
  expect_true(exists("cacheEnv"))
})


test_that("Does cacheEnv include pwd?", {
  #  expect_equal(cacheEnv$pwd$entityId, "robert_oracle-1:ST-63678")
  improveConnect()
  expect_true(exists("pwd", envir = cacheEnv))
})

