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

  # The configured step is the anchor every fixture hangs off. If it does not
  # resolve, loadResource() returns NULL, NULL$entityId is NULL, and
  # Sys.setenv() rejects it with "wrong length for argument" - a message that
  # names neither the variable nor the cause. Seen on 2026-09-14 after the
  # repository prefix in IMPROVER_STEP went stale (IMR-277).
  configuredStep <- Sys.getenv("IMPROVER_STEP")
  stepResource <- loadResource(configuredStep)
  if (is.null(stepResource) || is.null(stepResource$entityId)) {
    err <- tryCatch(lastRestError(), error = function(e) NULL)
    detail <- if (is.null(err)) "improveR recorded no REST error" else
      sprintf("HTTP %s on %s %s", err$status_code, err$method, err$url)
    stop(sprintf(paste0("IMPROVER_STEP does not resolve on this server: '%s' - %s\n",
                        "This is a run configuration problem. Note that the repository ",
                        "prefix is taken from this value and never checked against the ",
                        "server (repoPrefix, R/improveConnect.R). See test-preconditions.R."),
                 configuredStep, detail), call. = FALSE)
  }
  Sys.setenv(IMPROVER_STEP = stepResource$entityId)

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


# TRUE when the folder has no children yet.
#
# The two setup functions below used to ask
#     if (nrow(loadChildResources(path)$data[[1]]) == 0)
# which is exactly backwards: on an empty folder the loader yields nothing,
# nrow(NULL) is NULL, and `if (NULL == 0)` aborts with "argument is of length
# zero". The branch could therefore not handle the one state it exists to
# detect - it only worked while the folder was already populated, i.e. when its
# body was not needed.
#
# Measured consequence (IMR-271): in the first full grid run on 2026-09-09 the
# first ten files passed and every later one died here, once something had
# emptied the folder. The runner clears /Projects/Tests at the start of every
# run, so a fixture that cannot build from empty makes each run depend on its
# predecessor.
# loader is injectable so the condition can be tested without a server.
isFolderEmpty <- function(folderPath, loader = loadChildResources) {
  children <- tryCatch(loader(folderPath)$data[[1]], error = function(e) NULL)
  is.null(children) || NROW(children) == 0
}

baseFilesSetup <- function() {


  baseFilePath<-createFolderPath("baseFiles")
  if (isFolderEmpty(baseFilePath)) {
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

  if (isFolderEmpty(runFiles)) {

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

