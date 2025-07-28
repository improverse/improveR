# Checks if httptest (and only httptest) ic currently capturing the results by checking the trace on httr::POST


isCapturing <- function() {
  any(grepl("functionWithTrace",utils::capture.output(httr::POST)))
}


createFolderPath <- function(setupType) {

  TEST_FOLDER <- get0(setupType,envir=cacheEnv)
  if (!is.null(TEST_FOLDER)) {
    return(TEST_FOLDER)
  }

  path <- paste(
    Sys.getenv("TEST_FOLDER"),
    setupType,
    #stringr::str_replace_all(Sys.time(),":","-"),
    Sys.getenv("TEST_NAME"),
    sep="/"
  )

  folderSegments <- strsplit(path,split = "/",fixed=T)[[1]]

  stepEntity <- loadResource(Sys.getenv("IMPROVER_STEP"))$entityId
  Sys.setenv(IMPROVER_STEP=stepEntity)

  root <- loadResource("/")
  for (i in 2:length(folderSegments)) {
    folderSegment <- folderSegments[i]
    root <- createFolder(targetIdent=root,folderName=folderSegment)
  }
  assign(setupType,path,envir=cacheEnv)
  return(path)
}

emptyFolderSetup <- function() {
  TESTFOLDER_PATH<-createFolderPath("emptyFolder")
  unlink(".improve.json")
  return(TESTFOLDER_PATH)
}


baseFilesSetup <- function() {


  baseFilePath<-createFolderPath("baseFiles")
  if (nrow(loadChildResources(baseFilePath)$data[[1]])==0) {
    testZip <- system.file("testData.zip", package = "improveR")
    utils::unzip(testZip)
    TEMP_FOLDER_NAME <- "rgetTest"
    TEMP_FOLDER <- createFolder(baseFilePath,TEMP_FOLDER_NAME)

    GRAPH_FOLDER_NAME <- "rgetGRAPH"
    GRAPH_FOLDER <- createFolder(baseFilePath,GRAPH_FOLDER_NAME)

    HTML_FOLDER_NAME <- "rgetHTML"
    HTML_FOLDER <- createFolder(baseFilePath,HTML_FOLDER_NAME)

    TEXT_FOLDER_NAME <- "rgetTEXT"
    TEXT_FOLDER <- createFolder(baseFilePath,TEXT_FOLDER_NAME)

    R_FOLDER_NAME <- "rgetR"
    R_FOLDER <- createFolder(baseFilePath,R_FOLDER_NAME)


    f<-createFile(targetIdent = TEMP_FOLDER,fileName = "csv.csv",localPath = "./testData/csv.csv",comment = "Commit comment")
    f<-createFile(targetIdent = TEMP_FOLDER,fileName = "excel.xlsx",localPath = "./testData/excel.xlsx")
    f<-createFile(targetIdent = GRAPH_FOLDER,localPath = "./testData/image.JPG")
    f<-createFile(targetIdent = GRAPH_FOLDER,localPath = "./testData/front_kl.jpg")
    f<-createFile(targetIdent = GRAPH_FOLDER,localPath = "./testData/uploads.png")
    f<-createFile(targetIdent = HTML_FOLDER,localPath = "./testData/htmlExample.html")
    f<-createFile(targetIdent = TEXT_FOLDER,localPath = "./testData/sampleText.txt")
    f<-createFile(targetIdent = TEXT_FOLDER,localPath = "./testData/sampleText.txt",fileName = "sampleLink")
    f<-createFile(targetIdent = TEXT_FOLDER,localPath = "./testData/sampleText.txt",fileName = "sampleCopy")
    f<-createFile(targetIdent = R_FOLDER,localPath = "./testData/test.R")

    unlink("testData",recursive = T)
    unlink(".improve.json")
  }


  return(baseFilePath)
}


workflowFilesSetup <- function() {

  runFiles<-createFolderPath("runFiles")

  if (nrow(loadChildResources(runFiles)$data[[1]])==0) {

    testZip <- system.file("ExampleWorkflow.zip", package = "improveR")
    utils::unzip(testZip)

    f<-createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/data.csv")
    f<-createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/DataManipulation.R")
    f<-createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/DataManipulation.Rmd")
    f<-createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/EDA.R")
    f<-createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/example-new.dat")
    f<-createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/report.R")
    f<-createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/report.Rmd")
    f<-createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/STEP1.ctl")
    f<-createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/test_lm_plot.R")
    f<-createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/testWorkflow.r")
    f<-createFile(targetIdent = runFiles,localPath = "./ExampleWorkflow/testWorkflow.Rmd")


    unlink("ExampleWorkflow",recursive = T)
    unlink(".improve.json")
  }
  return(runFiles)

}

