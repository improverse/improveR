#' loadResourceByPathGeneric
#' @description Loads a resource by its path.
#' @param path path to the resource.
#' @param from working directory from which relative path is resolved.
#' Default is the root directory.
#' @noRd
loadResourceByPathGeneric <- function(path,from=pwd()) {
  logging::logdebug("loading path: ")
  logging::logdebug(path)
  returnResult <- F
  path <- stringr::str_replace_all(path,"\\\\","/")
  pathParts <- stringr::str_split(path,"/")[[1]]
  pathThatMatters <-pathParts[1]
  cwd <- NULL
  if (pathThatMatters=="") {
    cwd <- getRoot()
  } else {
    from <- loadResource(getCorrectId(from))
    cwd <- from

    #if (pathThatMatters=="..") {
    #  cwd <- loadResource(
    #    getParent(cwd)
    #  )
    #}
    #else if (pathPart=="...") {
    #  cwd <- getParentStep(cwd)
    #}
    #else {

    children <- loadChildResources(cwd$resourceId)$data[[1]]
    resourceId <- children[children$name==pathThatMatters,]$resourceId
    if (length(resourceId)==0) {
      log_warn("Resource with path:",paste0(cwd$path,"/",path),"could not be loaded.",pathThatMatters,"not found in",cwd$path)
      return(NULL)
    }
    cwd<-loadResource(resourceId)
    #}
  }
  if (length(pathParts)==1 | pathParts[2]=="") {
    return(cwd)
  } else {
    pathParts<-pathParts[2:length(pathParts)]
    newPath <- paste(pathParts,collapse = "/")
    return(loadResourceByPathGeneric(newPath,from=cwd)) #CHECK
  }
}

#' Pwd
#' @description Returns the working directory as a data frame.
#' If no pwd is set, root is used.
#'
#' @return a data frame.
#' @export
pwd <- function() {
  if (is.null(get0(x = "pwd",envir = cacheEnv))) {  
    cacheEnv$pwd <- getRoot()
  }
  return(cacheEnv$pwd)
}
