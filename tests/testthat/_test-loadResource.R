# TODO TEST FAILS
# httptest::with_mock_dir("testloadResource",{
#   test_that("loadResource", {
#     Sys.setenv(IMPROVER_TEST_REPLAY="T")
#     Sys.setenv(IMPROVER_REPO_URL=mockUrl)
#     print(Sys.getenv("IMPROVER_REPO_URL"))
#     improveConnect()

#     loadResourceResult <- loadResource(ident = "5363FB37F2424980A49DC6FD8CACDED4") #resourceId of folder "tests" on 18118 repo
#     print(loadResourceResult)
#     expect_true(!is.null(loadResourceResult))
#     # expect_s3_class(loadResourceResult, "data.frame")
#     # expect_true(nrow(loadResourceResult)==1)
#   })
# })



# test_that("loadResource correctly loads resources", {


#   TEST_FOLDER <- baseFilesSetup()

#   folder <- improveR::loadResource(TEST_FOLDER)
#   expect_equal(folder$nodeType,"Folder")
#   expect_equal(folder$path,TEST_FOLDER)

# })

