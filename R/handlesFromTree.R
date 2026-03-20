



findContainerStep <- function(ident) {
  if (is.null(ident)) {
    return(NULL)
  }
  remoteResource <- loadResource(ident)
  #print(remoteResource)
  if (remoteResource$resourceId==0) {
    return(NULL)
  }
  #print(remoteResource$nodeType)
  if (remoteResource$nodeType=="Step") {
    return(remoteResource)
  }
  return(findContainerStep(remoteResource$parentId))
}

getRelativePath <- function(parentStep,ident) {
  identPath <- loadResource(ident)$path
  stepPath <- loadResource(parentStep)$path
  relativePath <- substr(identPath,nchar(stepPath)+1,nchar(identPath))
  relativePath <- paste0(".",relativePath)
  return(relativePath)
}

