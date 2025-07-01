path <- "C:/dev/git-repos/improVerse/improveR"
backup <- "/Projects/backups"

testAll <- function(path) {
  Sys.setenv(TZ='Europe/Vienna')
  Sys.setenv(TEST_SERVER="runTest")
  Sys.setenv(IMPROVER_STEP="envhost1.hc.scintecodev.internal-5310:FO-19756")
  Sys.setenv(IMPROVER_REPO_URL="http://envhost1.hc.scintecodev.internal:5310/repository")
  Sys.setenv(TEST_FOLDER = "/Projects/Tests")
  Sys.setenv(NONMEM_RUNSERVER="runserver")
  Sys.setenv(NONMEM_TOOL_INSTANCE="nonmem_7.4")
  Sys.setenv(NONMEM_TOOL="nonmem_7.5")
  Sys.setenv(R_RUNSERVER="runserver")
  Sys.setenv(R_TOOL_INSTANCE="rbatch")
  Sys.setenv(R_TOOL="R_4.2")



  #improveR::improveOAuth(Sys.getenv("IMPROVER_REPO_URL"),
  #                       shortEntityId =Sys.getenv("IMPROVER_STEP") ,
  #                       openBrowser = F)
  devtools::load_all(path=path)
  clearConnectionData()
  improveConnect()
  autoRefreshStart()
  autoRefreshRunning()
  testPath <- file.path(path,"./tests/testthat")

  testthat::test_dir("./tests/testthat", reporter=c("minimal", "location"))

  httptestDirs <- dir(testPath,full.names = TRUE)
  httptestDirs<-httptestDirs[dir.exists(httptestDirs)]
  unlink(httptestDirs,recursive = T,force = T)
  improveR::move(Sys.getenv("TEST_FOLDER"),backup)
}


