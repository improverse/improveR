configEnv <- new.env()

#' cleans the environment
#'
#' @export
testInit <- function() {
  print("clean env")
  rm(list = ls(envir = globalenv()),envir = globalenv())
  if(Sys.getenv("TEST_SERVER")=="") {
    initRepo()
    credentialsFunctionName <- paste0(
      "setup",
      Sys.getenv("TEST_SERVER")
    )
    credentialsFunction <- match.fun(credentialsFunctionName)
    credentialsFunction()
  } 
  Sys.setenv(improver.logfile="improver.log")
  prepareConnect()
}

#' executeFolder
#' @description  Executes all Rmd-files in the folder specified by path. If available,
#' execution of the Rmd-files is preceeded by the execution of an init.R file and/or
#' followed by the execution of an cleanup.R file.
#' @param  path path to the executable package
#' @export
executeFolder <- function(path) {
  startWd <- getwd()
  RUN_FOLDER <- get("RUN_FOLDER",envir=configEnv)
  tryCatch(
    {
      assign("RELATIVEPATH",path,envir=configEnv)
      #setwd(path)
      folderName <- basename(getwd())
      sourceIfExists("init.R")
      files <- sort(dir("./"))
      files <- files[grepl("*.Rmd",files)]
      if (length(files)>0) {
        for (i in 1:length(files)) {
          inputFile <- files[i]
          outputFile <- paste0(inputFile,".html")
          rmarkdown::render(files[i],output_file = outputFile,envir = configEnv)
          dir.create(paste(RUN_FOLDER,folderName,sep="/"),recursive = T,showWarnings = F)
          file.rename(outputFile,paste(RUN_FOLDER,folderName,outputFile,sep="/"))
        }
      }
    },
    finally = {
      tryCatch(
        {
          sourceIfExists("cleanup.R")
        },
        finally = {
          print("finish")
          testInit()
        }
      )
    }
  )

}

sourceIfExists <- function(name) {
  files <- dir("./")
  if (name %in% files) {
    source(name)
  }
}

adaptPath <- function(path) {
  return(
    file.path(
      get("RELATIVEPATH",envir=configEnv),
      path
    )
  )
}

prepareConnect <- function() {




  TESTFOLDER_PATH <- paste(
    Sys.getenv("TEST_FOLDER"),Sys.getenv("TEST_SERVER"),sep="/"
  )

  assign("TESTFOLDER_PATH",TESTFOLDER_PATH,envir=configEnv)
  assign("RUN_FOLDER",
         paste(getwd(),
         TESTFOLDER_PATH,
         sep="/")
         ,envir=configEnv)
}

#' initRepo
#' @description Set runName and testServer to environment.
#' @param  runName name for the result folder
#' @param testServer a registered server
#' @examples
#' \dontrun{
#' initRepo(runName="TestFolder", testServer="TestServer") }
#' @export
#initRepo<-function(runName="test1",testServer="SWBEnv24") {
initRepo<-function(runName="test1",testServer="RobertEnv") {
# initRepo<-function(runName,testServer) {
  Sys.setenv(RUN_NAME = runName)
  Sys.setenv(TEST_SERVER = testServer)
}
