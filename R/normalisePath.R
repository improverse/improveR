#' normalisePath converts any path to an absolute path, so it eleminates all .. and .
#' if a relative path is given it needs an absolute path as startPath to resolve
#' if by too many .. the path navigates beyond root NULL is returned
#' examples:
#'   path<-"./../lmer/../lmer"
#' startPath <- "/0demo/lmer"
#' expected: /0demo/lmer
#'
#' path<-"./../../../lmer/../lmer"
#' startPath <- "/0demo/lmer"
#' expected: NULL
#'
#' path<-"/0demo/lmer"
#' startPath <- "/0demo/lmer"
#' expected: /0demo/lmer
#'
#' path<-"/0demo/lmer/"
#' startPath <- "/0demo/lmer"
#' expected: /0demo/lmer
#' @param path the path to normalise
#' @param startPath startpoint for relative pathes, defaults to /
#' @references ics1089
#' @export

normalisePath <- function(path,startPath="/") {
  if(is.data.frame(startPath)) {
    startPath<-startPath$path
  } else {
    if (is.null(startPath) | length(startPath)==0 | length(startPath)>1) {
      logging::logwarn(paste0(startPath," is an illegal relativeRoot, exactly one relativeRoot can be provided, using / instead"))
      startPath<-"/"
    } else {
      if (!startsWith(startPath,".") & !startsWith(startPath,"\\") & !startsWith(startPath,"/")) {
        startPath <- loadResource(startPath)$path
      }
    }
  }

  #if (nchar(path)>2 & endsWith(path,"/"))
  logging::logdebug("normalising path: ")
  logging::logdebug(path)

  path <- stringr::str_replace_all(path,"\\\\","/")
  path <- stringr::str_replace_all(path,"//","/")
  startPath <- stringr::str_replace_all(startPath,"\\\\","/")
  startPath <- stringr::str_replace_all(startPath,"//","/")
  if (nchar(path)>2 & endsWith(path,"/")) {
    path<-substr(path,1,nchar(path)-1)
  }
  pathParts <- stringr::str_split(path,"/")[[1]]
  pathThatMatters <-pathParts[1]
  if (pathThatMatters=="") {
    startPath <- ("/")
  } else if (pathThatMatters==".") {
    startPath <- startPath
  } else if (pathThatMatters=="..") {
    startPathParts <- stringr::str_split(startPath,"/")
    if (length(startPathParts)==1 && length(startPathParts[[1]])>1) {
      startPathParts <- startPathParts[[1]]
      startPathParts<-startPathParts[1:length(startPathParts)-1]
      startPath <- paste(startPathParts,collapse = "/")
    } else {
      logging::loginfo("trying to navigate beyond root:")
      logging::loginfo(path)
      logging::loginfo(startPath)
      return(NULL)
    }
  } else {
    startPath <- paste(startPath,pathThatMatters,sep="/")
  }
  if (length(pathParts)==1) {
    startPath <- stringr::str_replace_all(startPath,"//","/")
    return(startPath)
  } else {
    pathParts<-pathParts[2:length(pathParts)]
    newPath <- paste(pathParts,collapse = "/")
    return(normalisePath(newPath,startPath))
  }
}
