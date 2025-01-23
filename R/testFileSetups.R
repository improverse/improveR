emptyFolderSetup <- function() {
  TEST_FOLDER <- get0("EMPTY_FOLDER",envir=configEnv)
  if (!is.null(TEST_FOLDER)) {
    return(TEST_FOLDER)
  }

  testInit()
  TESTFOLDER_PATH<-createFolderPath("emptyFolder")
  unlink(".improve.json")
  return(TESTFOLDER_PATH)
}

baseFilesSetup <- function() {
  TEST_FOLDER <- get0("BASE_FILES",envir=configEnv)
  if (!is.null(TEST_FOLDER)) {
    return(TEST_FOLDER)
  }

  testInit()
  TESTFOLDER_PATH<-createFolderPath("baseFiles")

  assign("BASE_FILES",TESTFOLDER_PATH,envir=configEnv)


  testZip <- system.file("testData.zip", package = "improveR")
  utils::unzip(testZip)

  improveRcore::improveConnect(secure = F,logLevel = "INFO")
  improveConnect(secure = F,logLevel = "INFO")
  #test to log into file
  logging::addHandler(logging::writeToFile,logger="",file="improver.log")

  TESTFOLDER <- improveRmodify::createFolder(TESTFOLDER_PATH)
  TEMP_FOLDER_NAME <- "rgetTest"
  TEMP_FOLDER <- improveRmodify::createFolder(TESTFOLDER,TEMP_FOLDER_NAME)

  GRAPH_FOLDER_NAME <- "rgetGRAPH"
  GRAPH_FOLDER <- improveRmodify::createFolder(TESTFOLDER,GRAPH_FOLDER_NAME)

  HTML_FOLDER_NAME <- "rgetHTML"
  HTML_FOLDER <- improveRmodify::createFolder(TESTFOLDER,HTML_FOLDER_NAME)

  TEXT_FOLDER_NAME <- "rgetTEXT"
  TEXT_FOLDER <- improveRmodify::createFolder(TESTFOLDER,TEXT_FOLDER_NAME)

  R_FOLDER_NAME <- "rgetR"
  R_FOLDER <- improveRmodify::createFolder(TESTFOLDER,R_FOLDER_NAME)


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
  return(TESTFOLDER_PATH)
}


workflowFilesSetup <- function() {
  TEST_FOLDER <- get0("RUN_FILES",envir=configEnv)
  if (!is.null(TEST_FOLDER)) {
    return(TEST_FOLDER)
  }

  testInit()
  TESTFOLDER_PATH<-createFolderPath("runFiles")

  assign("RUN_FILES",TESTFOLDER_PATH,envir=configEnv)


  testZip <- system.file("ExampleWorkflow.zip", package = "smokeTests")
  utils::unzip(testZip)

  improveRcore::improveConnect(secure = F,logLevel = "INFO")



  TESTFOLDER <- improveRmodify::createFolder(TESTFOLDER_PATH)



  f<-improveRmodify::createFile(targetIdent = TESTFOLDER_PATH,localPath = "./ExampleWorkflow/data.csv")
  f<-improveRmodify::createFile(targetIdent = TESTFOLDER_PATH,localPath = "./ExampleWorkflow/DataManipulation.R")
  f<-improveRmodify::createFile(targetIdent = TESTFOLDER_PATH,localPath = "./ExampleWorkflow/DataManipulation.Rmd")
  f<-improveRmodify::createFile(targetIdent = TESTFOLDER_PATH,localPath = "./ExampleWorkflow/EDA.R")
  f<-improveRmodify::createFile(targetIdent = TESTFOLDER_PATH,localPath = "./ExampleWorkflow/example-new.dat")
  f<-improveRmodify::createFile(targetIdent = TESTFOLDER_PATH,localPath = "./ExampleWorkflow/report.R")
  f<-improveRmodify::createFile(targetIdent = TESTFOLDER_PATH,localPath = "./ExampleWorkflow/report.Rmd")
  f<-improveRmodify::createFile(targetIdent = TESTFOLDER_PATH,localPath = "./ExampleWorkflow/STEP1.ctl")
  f<-improveRmodify::createFile(targetIdent = TESTFOLDER_PATH,localPath = "./ExampleWorkflow/test_lm_plot.R")
  f<-improveRmodify::createFile(targetIdent = TESTFOLDER_PATH,localPath = "./ExampleWorkflow/testWorkflow.r")
  f<-improveRmodify::createFile(targetIdent = TESTFOLDER_PATH,localPath = "./ExampleWorkflow/testWorkflow.Rmd")


  unlink("ExampleWorkflow",recursive = T)
  unlink(".improve.json")
  return(TESTFOLDER_PATH)

}


createFolderPath <- function(setupType) {
  path <- paste(
    Sys.getenv("TEST_FOLDER"),
    setupType,
    stringr::str_replace_all(Sys.time(),":","-"),
    sep="/"
  )

  folderSegments <- strsplit(path,split = "/",fixed=T)[[1]]
  improveRcore::improveConnect(secure=F)

  stepEntity <- improveRcore::loadResource(Sys.getenv("IMPROVER_STEP"))$entityId
  Sys.setenv(IMPROVER_STEP=stepEntity)

  root <- improveRcore::loadResource("/")
  for (i in 2:length(folderSegments)) {
    folderSegment <- folderSegments[i]
    root <- improveRmodify::createFolder(targetIdent=root,folderName=folderSegment)
  }
  improveRcore::improveDisconnect()
  return(path)
}
