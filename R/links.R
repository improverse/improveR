#' createLink
#' creates a link to a resource, currently always the latest version of the resource is used
#' @param linkContainer where to create the link
#' @param links to which resource to link
#' @param linkName name for the link, if this is not given the name of the linked resource is used. Can only be used if only one resource is linked
#' @references ics1138
#' @export
createLink <- function(linkContainer,links,linkName="") {
  improveEditable()
  if (!is.character(linkName)) {
    log_warn("name needs to be of type character")
    return(NULL)
  }
  if (linkName =="") {
    linkName<-NULL
  }
  target <- loadResource(linkContainer)
  if (is.null(target)) {
    log_warn("Target does not exist")
    return(NULL)
  }
  links <- loadResource(links)
  if (is.null(links)) {
    log_warn("LinkTarget does not exist")
    return(NULL)
  }
  if (nrow(target)>1) {
    return(Map(function(targ) {
      return(createLink(targ,links,linkName))
    },target$path))
  } else if (nrow(target)==1) {
    if (nrow(links)>1) {
      return(Map(function(lin) {
        return(createLink(target,lin,NULL))
      },links))
    } else {
      targetChildren <- loadChildResources(target)$data[[1]]

      #TODO more checks
      if (is.null(linkName)) {
        linkName<-links$name
      }
      if (!isAllowedTarget(target$nodeType,"Link",logWarning=T)) {
        return(NULL)
      }
      exists <- targetChildren[targetChildren$name==linkName,]



      if (nrow(exists)==1) {
        if (exists$nodeType=="Link") {
          log_info(linkName,"already exists in",target$path)
          return(loadResource(exists))
        }
        log_warn(linkName,"already exists in",target$path,"but is of type",exists$nodeType)
        return(NULL)
      }
      data<-list(name=linkName,targetId=target$resourceId)
      result <- authenticatedREST('/resources/{resourceId}/references',
                                                urlParams = list(resourceId=links$resourceId),
                                                data=data,
                                                restType = "POST")

      unloadChildResources(target)
      unloadFullChildResources(target)
      unloadReferences(links)
      res <- loadResource(paste0("./",linkName),from = target)
      return(res)
    }
  } else {
    logging::logwarn(paste0(linkContainer," does not exist"))
  }

}



