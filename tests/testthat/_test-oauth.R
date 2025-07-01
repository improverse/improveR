





httptest::with_mock_dir("fullOauthDirectSuccess",{
  test_that("fullOauthDirectSuccess", {

    authenticationProvider <- getAuthenticationProvider(testUrl)
    expect_true(length(authenticationProvider)==7)
    expect_equal("improve-api-client",authenticationProvider$clientId)
    ########################
    cacheEnv$codeVerifier <- "WUsHGZRCV9NGaRfp9RlaMl4NQvLx8TNtrUj5crJYXH7wTJcaxt4ykP7AAJ41kVtICGfmzdUacdACgQ6y5OlTz6bt9CX1Hc5oyb6F6K5ovlPAQ-GuRBdZlOGw4vsoXSas"
    initiatedAuthentication <- startOAuth(authenticationProvider,withCodeVerifier=T)
    expect_true(all(
      c("device_code","user_code","verification_uri","verification_uri_complete","expires_in","interval")
          %in%
          names(initiatedAuthentication))
    )
    instructions <- utils::capture.output(
      initiatedAuthentication <-showOAuth(initiatedAuthentication,openBrowser = F)
    )
    expect_equal(length(instructions),1)




    #authenticatedResult <- hasAuthenticated(initiatedAuthentication)
    #httptest::change_state()
    if (isCapturing()) {
      print("login within 30 seconds")
      print(instructions)
      Sys.sleep(30)
    }
    authenticatedResult <- hasAuthenticated(initiatedAuthentication)
    expect_equal(authenticatedResult$status_code,200)
    authenticatedContent <- httr::content(authenticatedResult)
    expect_true(all(
      c("access_token","expires_in","refresh_expires_in","refresh_token","id_token")
      %in%
        names(authenticatedContent)
    ))


    user <- jose::jwt_split(authenticatedContent$id_token)$payload$preferred_username
    expect_true(is.character(user))



    Sys.setenv(IMPROVER_REPO_URL=testUrl)
    Sys.setenv(IMPROVER_USER=user)
    Sys.setenv(IMPROVER_TOKEN=authenticatedContent$access_token)
    Sys.setenv(IMPROVER_STEP="/")


    Sys.setenv(IMPROVER_TOKEN_EXPIRATION=authenticatedContent$expires_in)
    Sys.setenv(IMPROVER_REFRESH_TOKEN=authenticatedContent$refresh_token)
    Sys.setenv(IMPROVER_LAST_ACCESS=as.numeric(Sys.time()))


    improveConnect("INFO",secure=F)

    #rootFolders <- loadChildResources("/")$data[[1]]
    #expect_true(nrow(rootFolders)>0)

    renewAccessToken()
    expect_false(authenticatedContent$access_token==Sys.getenv("IMPROVER_TOKEN"))
  })
})

httptest::with_mock_dir("simpleOauth",{
  test_that("simpleOauth", {
    clearConnectionData()
    Sys.setenv(IMPROVER_TEST_REPLAY="T")
    improveOAuth(testUrl)
    expect_false(Sys.getenv("IMPROVER_TOKEN")=="")
    improveRevokeOAuth()
    expect_true(Sys.getenv("IMPROVER_TOKEN")=="")
  })
})


httptest::with_mock_dir("simpleConnectWithOauth",{
  test_that("simpleConnectWithOauth", {
    clearConnectionData()
    Sys.setenv(IMPROVER_TEST_REPLAY="T")
    Sys.setenv(IMPROVER_REPO_URL=testUrl)
    improveConnect()
    expect_false(Sys.getenv("IMPROVER_TOKEN")=="")
    improveRevokeOAuth()
    expect_true(Sys.getenv("IMPROVER_TOKEN")=="")
  })
})



httptest::with_mock_dir("simpleStubWithLogin",{
  test_that("simpleStubWithLogin", {
    clearConnectionData()
    Sys.setenv(IMPROVER_TEST_REPLAY="T")
    Sys.setenv(IMPROVER_REPO_URL=testUrl)
    improveConnect()
    expect_false(Sys.getenv("IMPROVER_TOKEN")=="")
    users <- authenticatedREST("/users")
  })
})

# for capturing it is important that you have logged in before to the repo
# but I think it is not necessary to test the login process every time when capturing
httptest::with_mock_dir("simpleStubLoggedIn",{
  test_that("simpleStubLoggedIn", {
    improveConnect()
    expect_false(Sys.getenv("IMPROVER_TOKEN")=="")
    users <- authenticatedREST("/users")
  })
})


httptest::with_mock_dir("simpleRefresh",{
  test_that("simpleRefresh", {
    if (F) {
      clearConnectionData()
      Sys.setenv(IMPROVER_TEST_REPLAY="T")
      improveOAuth(mockUrl)
      expect_false(Sys.getenv("IMPROVER_TOKEN")=="")
      oldToken <- Sys.getenv("IMPROVER_TOKEN")
      Sys.sleep(330)
      users <- authenticatedREST("/users")
      expect_false(Sys.getenv("IMPROVER_TOKEN")==oldToken)
    }
  })
})

testthat::test_that("tests the creation of a correct code challenge",{
    code_verifier <- "WUsHGZRCV9NGaRfp9RlaMl4NQvLx8TNtrUj5crJYXH7wTJcaxt4ykP7AAJ41kVtICGfmzdUacdACgQ6y5OlTz6bt9CX1Hc5oyb6F6K5ovlPAQ-GuRBdZlOGw4vsoXSas"
    #example from working reference implementation
    compareCodeChallenge <-"0p4FWNgRCuN6RgIo-NBwklGQPIBvdkd6oXe5PAeZMog"
    code_challenge <- createCodeChallenge(code_verifier)
    testthat::expect_equal(code_challenge, compareCodeChallenge)
})


