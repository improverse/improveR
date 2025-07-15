

#adapt names for runserver, ...
#complete toolParameters
#resetToolParameters

toolInstanceCache <- new.env()

#' resetToolInstances
#' clears the cache for toolInstances and reloads everything from the server
#' @export
resetToolInstances <- function() {
  rm(list=ls(envir = toolInstanceCache),envir = toolInstanceCache)
}

getGridValues <- function() {
  if (exists(x = "gridValues",envir = toolInstanceCache)) {
    return(get(x = "gridValues",envir = toolInstanceCache))
  } else {
    log_info("load gridValues")
    gridCategoriesResult <- authenticatedREST("/configuration/gridArguments/categories")
    gridCategories<-mergeNestedListToDataframe(httr::content(gridCategoriesResult))

    gridValues <- byNotEmptyAsDf(gridCategories,function(gridCategory) {
      return(gridCategory$values[[1]])
    })
    assign(x = "gridValues",value = gridValues,envir = toolInstanceCache)
    return(gridValues)
  }
}

getParameterValues <- function() {
  if (exists(x = "parameterValues",envir = toolInstanceCache)) {
    return(get(x = "parameterValues",envir = toolInstanceCache))
  } else {
    log_info("load parameterValues")
    parameterResult <- authenticatedREST("/configuration/parameterLov")
    parameters <- mergeListToDataframe(httr::content(parameterResult))
    assign(x = "parameterValues",value = parameters,envir = toolInstanceCache)
    return(parameters)
  }
}

#' getToolInstances
#' returns an environement with all tool instances. in order to relaod tools from the server use resetToolInstances
#' @export
getToolInstances <- function() {

  if (exists(x = "toolInstances",envir = toolInstanceCache)) {
    return(get(x = "toolInstances",envir = toolInstanceCache))
  } else {
    log_info("load toolInstances")
    gridValues <- getGridValues()
    parameters <- getParameterValues()

    runservers <- loadRunservers()
    runservers <- dplyr::filter(runservers,!local)
    runserverTools <- byNotEmptyAsDf(runservers,function(runserver) {
      runServerTools <- loadToolsForRunserver(runserver$id)
      runServerTools <- byNotEmptyAsDf(runServerTools,function(runserverTool) {
        #####parameters
        toolParameterResult<-improveR::authenticatedREST("/configuration/runservers/{runserverId}/tools/{toolId}/parameters",
                                                         urlParams = list(runserverId=runserverTool$runserverId,toolId=runserverTool$id))
        toolParameters <- mergeListToDataframe(httr::content(toolParameterResult))
        toolParameters <- dplyr::select(
          dplyr::inner_join(parameters,toolParameters,by=c("id"="parameterLovId"))
          ,lovType,name,description,value)
        runserverTool$parameters <- list(toolParameters)
        ####gridArguments
        tryCatch( {
          gridArgumentDefinitions <- loadGridArguments(runserverTool$gridProvider)
          gridArgumentResult<-improveR::authenticatedREST("/configuration/runservers/{runserverId}/tools/{toolId}/gridArguments",
                                                          urlParams = list(runserverId=runserverTool$runserverId,toolId=runserverTool$id))
          gridArguments <- mergeListToDataframe(httr::content(gridArgumentResult))
          if (nrow(gridArguments)>0) {
            merged <- dplyr::inner_join(gridArgumentDefinitions,gridArguments,by=c("id"="definitionId"))
            mergedValues <- dplyr::left_join(merged,gridValues,c("lovValueId"="id"))
            if (!("textValue" %in% names(mergedValues))) {
              mergedValues$textValue <- NA
            }
            if (!("text" %in% names(mergedValues))) {
              mergedValues$text <- NA
            }
            mergedValues <- dplyr::mutate(mergedValues,value=dplyr::if_else(is.na(textValue),text,textValue))
            gridArguments <- dplyr::select(mergedValues,name,value)
            runserverTool$gridArguments <- list(gridArguments)
          }

        },error=function(e){}
        )

        return(runserverTool)
      })

      return(runServerTools)
    })
    runserverTools <- dplyr::mutate(runserverTools,fullName=paste(categoryName,toolName,name,label))
    toolInstanceEnv <- new.env()
    x <- byNotEmpty(runserverTools,function(runserverTool) {
      assign(x=runserverTool$fullName,value=runserverTool,envir =toolInstanceEnv )
    })
    assign(x = "toolInstances",value = toolInstanceEnv,envir = toolInstanceCache)
    return(toolInstanceEnv)

  }



}
