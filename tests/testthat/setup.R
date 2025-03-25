#this file is using old improveRcore and improveRmodify functions. needs to be changed when improveRW is working

library(httptest)

mockUrl <- Sys.getenv("IMPROVER_TEST_REPO")
if (mockUrl=="") {
  mockUrl <- "http://envhost2.hc.scintecodev.internal:18118/repository"
}
#mockUrl <- "http://10.0.0.2:18118/repository"

TESTFOLDER_ROOT <- Sys.getenv("TESTFOLDER_ROOT")
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
  rootFolder <- improveRcore::loadResource("/")

  for (i in 1:length(pathParts)) {
    rootFolder <- improveRmodify::createFolder(rootFolder,folderName = pathParts[i])
  }
  return(rootFolder)
}

createFolderPath <- function(testRootFolder,setupType) {
  path <- paste(
    setupType,
    stringr::str_replace_all(Sys.time(),":","-"),
    sep="/"
  )
  folderSegments <- strsplit(path,split = "/",fixed=T)[[1]]
  root <- testRootFolder
  for (i in 1:length(folderSegments)) {
    folderSegment <- folderSegments[i]
    root <- improveRmodify::createFolder(targetIdent=root,folderName=folderSegment)
  }
  return(root$path)
}

emptyFolderSetup <- function(testRootFolder) {
  emptyPath<-createFolderPath(testRootFolder,"emptyFolder")
  return(emptyPath)
}


baseFilesSetup <- function(testRootFolder) {


  baseFilePath<-createFolderPath(testRootFolder,"baseFiles")


  testZip <- system.file("testData.zip", package = "improveR")
  utils::unzip(testZip)
  TEMP_FOLDER_NAME <- "rgetTest"
  TEMP_FOLDER <- improveRmodify::createFolder(baseFilePath,TEMP_FOLDER_NAME)

  GRAPH_FOLDER_NAME <- "rgetGRAPH"
  GRAPH_FOLDER <- improveRmodify::createFolder(baseFilePath,GRAPH_FOLDER_NAME)

  HTML_FOLDER_NAME <- "rgetHTML"
  HTML_FOLDER <- improveRmodify::createFolder(baseFilePath,HTML_FOLDER_NAME)

  TEXT_FOLDER_NAME <- "rgetTEXT"
  TEXT_FOLDER <- improveRmodify::createFolder(baseFilePath,TEXT_FOLDER_NAME)

  R_FOLDER_NAME <- "rgetR"
  R_FOLDER <- improveRmodify::createFolder(baseFilePath,R_FOLDER_NAME)


  f<-improveRmodify::createFile(targetIdent = TEMP_FOLDER,fileName = "csv.csv",localPath = "./testData/csv.csv",comment = "Commit comment")
  f<-improveRmodify::createFile(targetIdent = TEMP_FOLDER,fileName = "excel.xlsx",localPath = "./testData/excel.xlsx")
  f<-improveRmodify::createFile(targetIdent = GRAPH_FOLDER,localPath = "./testData/image.JPG")
  f<-improveRmodify::createFile(targetIdent = GRAPH_FOLDER,localPath = "./testData/front_kl.jpg")
  f<-improveRmodify::createFile(targetIdent = GRAPH_FOLDER,localPath = "./testData/uploads.png")
  f<-improveRmodify::createFile(targetIdent = HTML_FOLDER,localPath = "./testData/htmlExample.html")
  f<-improveRmodify::createFile(targetIdent = TEXT_FOLDER,localPath = "./testData/sampleText.txt")
  f<-improveRmodify::createFile(targetIdent = TEXT_FOLDER,localPath = "./testData/sampleText.txt",fileName = "sampleLink")
  f<-improveRmodify::createFile(targetIdent = TEXT_FOLDER,localPath = "./testData/sampleText.txt",fileName = "sampleCopy")
  f<-improveRmodify::createFile(targetIdent = R_FOLDER,localPath = "./testData/test.R")

  unlink("testData",recursive = T)
  unlink(".improve.json")
  return(baseFilePath)
}

workflowFilesSetup <- function(testRootFolder) {

  runFiles<-createFolderPath(testRootFolder,"runFiles")



  testZip <- system.file("ExampleWorkflow.zip", package = "improveR")
  utils::unzip(testZip)

  f<-improveRmodify::createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/data.csv")
  f<-improveRmodify::createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/DataManipulation.R")
  f<-improveRmodify::createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/DataManipulation.Rmd")
  f<-improveRmodify::createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/EDA.R")
  f<-improveRmodify::createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/example-new.dat")
  f<-improveRmodify::createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/report.R")
  f<-improveRmodify::createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/report.Rmd")
  f<-improveRmodify::createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/STEP1.ctl")
  f<-improveRmodify::createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/test_lm_plot.R")
  f<-improveRmodify::createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/testWorkflow.r")
  f<-improveRmodify::createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/testWorkflow.Rmd")


  unlink("ExampleWorkflow",recursive = T)
  unlink(".improve.json")
  return(runFiles)

}


httptest::with_mock_dir("setUpFiles",{
    clearConnectionData()
    Sys.setenv(IMPROVER_TEST_REPLAY="T")
    Sys.setenv(IMPROVER_REPO_URL=mockUrl)
    improveConnect()
    improveRcore::improveConnect()
    testRootFolder <- getOrCreateTestFolderRoot()
    folderPathes <- list()
    folderPathes$emptyFiles <- emptyFolderSetup(testRootFolder)
    folderPathes$baseFiles <- baseFilesSetup(testRootFolder)
    folderPathes$workflowFiles <- workflowFilesSetup(testRootFolder)
    assign(x = "testFolders",value = folderPathes,envir = globalenv())
})







