# TODO
# httptest::with_mock_dir("loadResourceByPathGeneric",{
#   test_that("loadResourceByPathGeneric", {
#     Sys.setenv(IMPROVER_TEST_REPLAY="T")
#     Sys.setenv(IMPROVER_REPO_URL=mockUrl)
#     print(Sys.getenv("IMPROVER_REPO_URL"))
#     improveConnect()
#     #loadResourceByPathGenericResult <- loadResourceByPathGeneric(path = "robert_oracle-1:ST-63657")

#     loadResourceByPathGenericResult <- loadResourceByPathGeneric(path = "envhost2.hc.scintecodev.internal-18118:FO-44271")
#     print(loadResourceByPathGenericResult)
#     expect_true(!is.null(loadResourceByPathGenericResult))
#     # expect_s3_class(loadResourceByPathGenericResult, "data.frame")
#     # expect_true(nrow(loadResourceByPathGenericResult)==1)
#   })
# })