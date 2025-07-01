
library(httptest)

testUrl <- Sys.getenv("IMPROVER_REPO_URL")


TESTFOLDER_ROOT <- Sys.getenv("TEST_FOLDER")


httptest::with_mock_dir("setUpFiles",{
    clearConnectionData()
    Sys.setenv(IMPROVER_TEST_REPLAY="T")
    Sys.setenv(IMPROVER_REPO_URL=testUrl)
    improveConnect()
    #setEditable()


})







