#' gets a R object
#'
#' @param ident path, resource or entity ID of the R object
#'
#' @param addAsLink creates a link in inventory if TRUE, warn improveClean has to be run at the end
#' @param from used for relative paths, by default pwd is used, which is initiated with the step that started improveR
#' @param caption by default entityID and lastmodified are the caption, here alternative text can be provided
#' @param description by default filename is the caption, here alternative text can be provided
#' @references ics1141
#' @export
getR <- function(ident,from=pwd(),addAsLink=TRUE,caption="",description="") {
  return(
    getAbstract(ident=ident,from = from,addAsLink = addAsLink,addIdToName = T,caption = caption,description=description,folderName = "R",func=getDesc)
  )
}


#' sources R objects
#'
#' @param ident path, resource or entity ID of the R object
#'
#' @param addAsLink creates a link in inventory if TRUE, warn improveClean has to be run at the end
#' @param from used for relative paths, by default pwd is used, which is initiated with the step that started improveR
#' @references ics1141
#' @export
sourceR <- function(ident,from=pwd(),addAsLink=TRUE) {

  rObject <- getR(ident=ident,from = from,addAsLink = addAsLink)
  if (is.null(rObject$path)) {
    if (length(rObject)==0) {
      logging::logerror("No R script found")
      logging::logerror(ident)
      return()
    }
    for (i in 1:length(rObject)) {
        sourceR(rObject[i][[1]]$resource,addAsLink = addAsLink)
    }
  }  else {

source(
  normalizePath(rObject$path,winslash = "/")
)
  }

}


#' initialises modules, also automatically connects
#'
#' @param ident path, resource or entity ID of the R object
#'
#' @param logLevel logLevel for improve connect
#' @param secure if certificates should be checked
#' @param from used for relative paths, by default from is used, which is initiated with the step that started improveR
#'
#' @export
improveInit <- function(ident,from=pwd(),logLevel="INFO",secure=T) {
  log_info("initialising module for ",ident)
  improveConnect(logLevel = logLevel,secure = secure)
  initFile <- loadResource(ident,from)
  if (is.null(initFile)) {
    log_error("no module definition found at",ident,from)
    return(NULL)
  }
  src <- sourceR(initFile)

  initFileBaseName<- strsplit(initFile$name,".",fixed = T)[[1]]
  if (length(initFileBaseName)<2) {
    log_error("There needs to be at least one . in the filename, ",initFile$name)
    return(NULL)
  }
  initFileBaseName <- paste(initFileBaseName[1:length(initFileBaseName)-1],collapse = ".")
  initFunctionName <- paste0(initFileBaseName,".init")
  myInit <-match.fun(initFunctionName)
  #getrootpath for further resolution
  directory <- loadResource(initFile$parentId)
  rootPath<-directory$path
  env1<-myInit(rootPath)
  return(env1)
}
