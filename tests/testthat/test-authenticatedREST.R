#testing authenticatedREST with 2 different test packes: httptest and webfakes
## test for different http status codes
## test for implications of different authentication methods
## test for different ident types

# HTTPTEST - loadResourceFromServer - shortEntityId

# TODO 
httptest::with_mock_dir("DoesloadResourceFromServerWork", {
# httptest::with_mock_api({
  test_that("Does loadResourceFromServer with shortEnttiyIdwork", {
   
    improveConnect()

    loadResourceFromServerResult <- loadResourceFromServer(resourceId = "robert_oracle-1:ST-63657")
    # loadResourceFromServerResult <- loadResourceFromServer(resourceId = "30A554E676F34AB8B1DE55CB5AE21E7A")

    testthat::expect_s3_class(loadResourceFromServerResult, "data.frame")
    # testthat::expect_true(nrow(loadResourceFromServerResult)==1)
  })
})


#TODO
# httptest::with_mock_dir("improveConnectTest",{
#   loadResourceFromServer(resourceId = "robert_oracle-1:ST-63657")
# })
