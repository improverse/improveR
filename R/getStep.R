getStep <- function(ident) {
  ident <- "envhost1.hc.scintecodev.internal-5310:ST-79162"

  step <- loadResource(ident)
  restResult <- authenticatedREST(url = "/resources/{resourceId}",urlParams = list(resourceId = step$resourceId))
  restContent <- httr::content(restResult)

  restResult <- authenticatedREST(url = "/resources/{resourceId}/resources",urlParams = list(resourceId = step$resourceId))

  restContent <- httr::content(restResult)


  handle <- handleFromStep(ident)

}


getToolInstances <- function() {
  runservers <- improveR::loadRunservers()
  runservers <- dplyr::filter(runservers,!local)
  runserverTools <- byNotEmptyAsDf(runservers,function(runserver) {
    return(loadToolsForRunserver(runserver$id))
  })
  runserverTools <- dplyr::mutate(runserverTools,fullName=paste(categoryName,toolName,name,label))
  toolInstanceEnv <- new.env()
  x <- byNotEmpty(runserverTools,function(runserverTool) {
    assign(x=runserverTool$fullName,value=runserverTool,envir =toolInstanceEnv )
  })
}
