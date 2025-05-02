Sys.setenv(IMPROVER_STEP="envhost1.hc.scintecodev.internal-5310:FO-54947")
Sys.setenv(IMPROVER_REPO_URL="http://envhost1.hc.scintecodev.internal:5310/repository")
Sys.setenv(TEST_FOLDER = "/Projects/Tests")


Sys.setenv(NONMEM_RUNSERVER="runserver")
Sys.setenv(NONMEM_TOOL_INSTANCE="nonmem_7.4")
Sys.setenv(NONMEM_TOOL="nonmem_7.5")

Sys.setenv(R_RUNSERVER="runserver")
Sys.setenv(R_TOOL_INSTANCE="rbatch")
Sys.setenv(R_TOOL="R_4.2")
Sys.setenv(TEST_SERVER="5310")

library(httptest)

mockUrl <- Sys.getenv("IMPROVER_REPO_URL")
if (mockUrl=="") {
  mockUrl <- "http://envhost1.hc.scintecodev.internal:5310/repository"
}
#mockUrl <- "http://10.0.0.2:18118/repository"

TESTFOLDER_ROOT <- Sys.getenv("TEST_FOLDER")
if (TESTFOLDER_ROOT=="") {
  TESTFOLDER_ROOT <- "/infra/tests/"
}

getOrCreateTestFolderRoot <- function() {
  if (!startsWith(x = TESTFOLDER_ROOT,prefix = "/")) {
    stop("environment variable TESTFOLDER_ROOT needs to be an absolut path, starting with /")
  }
  pathParts <- strsplit(TESTFOLDER_ROOT,"/",fixed=T)[[1]]
  if (length(pathParts)==1) {
    stop("environment variable TESTFOLDER_ROOT is not allowed to be the root folder")
  }
  pathParts <- pathParts[2:length(pathParts)]
  rootFolder <- loadResource("/")

  for (i in 1:length(pathParts)) {
    rootFolder <- createFolder(rootFolder,folderName = pathParts[i])
  }
  return(rootFolder)
}

createFolderPath <- function(testRootFolder,setupType) {

  folderName <- stringr::str_replace_all(Sys.time(),":","-")
  if (isCapturing() || Sys.getenv("IMPROVER_TEST_REPLAY")=="T") {
    folderName <- "httptestCapture"
  }

  path <- paste(
    setupType,
    folderName,
    sep="/"
  )



  folderSegments <- strsplit(path,split = "/",fixed=T)[[1]]
  root <- testRootFolder
  for (i in 1:length(folderSegments)) {
    folderSegment <- folderSegments[i]
    root <- createFolder(targetIdent=root,folderName=folderSegment)
  }
  return(root$path)
}

emptyFolderSetup <- function(testRootFolder) {
  emptyPath<-createFolderPath(testRootFolder,"emptyFolder")
  return(emptyPath)
}





httptest::with_mock_dir("setUpFiles",{
    clearConnectionData()
    Sys.setenv(IMPROVER_TEST_REPLAY="T")
    Sys.setenv(IMPROVER_REPO_URL=mockUrl)
    improveConnect()
    setEditable()
    testRootFolder <- getOrCreateTestFolderRoot()
    folderPathes <- list()
    folderPathes$emptyFiles <- emptyFolderSetup(testRootFolder)
    folderPathes$baseFiles <- baseFilesSetup(testRootFolder)
    folderPathes$workflowFiles <- workflowFilesSetup(testRootFolder)
    assign(x = "TEST_FOLDERS",value = folderPathes,envir = globalenv())
})







