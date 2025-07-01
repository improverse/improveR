# TEST
# httptest::with_mock_dir("loadResourceFromServerWorks", {
#
#   test_that("loadResourceFromServerWorks", {
#
#     #Folder
#
#     baseFileFolder <- loadResource(TEST_FOLDERS$baseFiles)
#
#
#     subFolders <- loadChildResources(baseFileFolder)$data[[1]]
#     subFolders$resourceId
#
#     Sys.setenv(IMPROVER_TEST_REPLAY="T")
#     Sys.setenv(IMPROVER_REPO_URL=mockUrl)
#     improveConnect()
#
#     testthat::expect_s3_class(loadResourceFromServerResult, "data.frame")
#     testthat::expect_true(nrow(loadResourceFromServerResult)==1)
#
#   })
# })

# # TEST WORKS WHEN RECORDING ON THE 4210 REPOSITORY
# httptest::with_mock_dir("loadResourceFromServerWorks", {

#   test_that("loadResourceFromServerWorks", {

#     improveConnect()

#     loadResourceFromServerResult <- loadResourceFromServer(resourceId = "robert_oracle-1:ST-63657")
#     testthat::expect_s3_class(loadResourceFromServerResult, "data.frame")
#     testthat::expect_true(nrow(loadResourceFromServerResult)==1)

#   })
# })








test_that("Does cacheEnv exist", {
  expect_true(exists("cacheEnv"))
})


test_that("Does cacheEnv include pwd?", {
  #  expect_equal(cacheEnv$pwd$entityId, "robert_oracle-1:ST-63678")
  improveConnect()
  expect_true(exists("pwd", envir = cacheEnv))
})

