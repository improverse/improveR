httptest::with_mock_dir("loadResourceByPathGeneric",{
  test_that("loadResourceByPathGeneric", {
    Sys.setenv(IMPROVER_TEST_REPLAY="T")
    Sys.setenv(IMPROVER_REPO_URL=mockUrl)
    improveConnect()
    loadResourceByPathGenericResult <- loadResourceByPathGeneric(path = "robert_oracle-1:ST-63657")
    expect_s3_class(loadResourceByPathGenericResult, "data.frame")
    expect_true(nrow(loadResourceByPathGenericResult)==1)
  })
})