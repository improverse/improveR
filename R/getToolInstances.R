

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



#' Get Tool Instances
#'
#' Returns an environment containing all available tool instances with their configurations.
#' Tools orchestrate step execution by defining how scripts are run, which external software
#' to use (R, NONMEM, Monolix), and how to handle inputs and outputs.
#'
#' @return An environment where each tool instance is accessible by its full name.
#'   Each tool instance contains:
#'   \describe{
#'     \item{toolId}{Unique identifier for the tool}
#'     \item{runserverId}{ID of the runserver hosting the tool}
#'     \item{url}{URL of the tool service}
#'     \item{parameters}{Data frame of tool parameters and their values}
#'     \item{gridArguments}{Data frame of grid computing arguments (if applicable)}
#'   }
#'
#' @details
#' The function checks for cached tool instances first, returning them immediately if
#' available. Otherwise, it loads tool configuration data from the server, including
#' runservers, tools, parameters, and grid arguments. Results are cached for performance.
#' When tool configurations change, call \code{\link{resetToolInstances}} to clear the cache.
#'
#' Tool instances are referenced in \code{stepDf$processes} within step environments
#' created by \code{getStep()}, determining how steps execute.
#'
#' @seealso
#' \code{\link{resetToolInstances}} to clear the tool instance cache,
#' \code{\link{getMainProcess}} to retrieve the tool instance for a specific step,
#' \code{\link{getStep}} for step environments that reference tool instances
#'
#' @examples
#' \dontrun{
#' # Get all tool instances
#' tools <- getToolInstances()
#'
#' # List available tools
#' ls(tools)
#'
#' # Access a specific tool instance
#' rTool <- tools$`R R R runserver`
#'
#' # View tool parameters
#' rTool$parameters
#' }
#'
#' @export
getToolInstances <- function() {

  if (exists(x = "toolInstances",envir = toolInstanceCache)) {
    return(get(x = "toolInstances",envir = toolInstanceCache))
  } else {
    log_info("load toolInstances")
    gridValues <- getGridValues()
    parameters <- getParameterValues()

    runservers <- loadRunservers()
    runservers <- dplyr::filter(runservers,!.data$local)
    runserverTools <- byNotEmptyAsDf(runservers,function(runserver) {
      runServerTools <- loadToolsForRunserver(runserver$id)
      runServerTools <- byNotEmptyAsDf(runServerTools,function(runserverTool) {
        #####parameters
        toolParameterResult<-authenticatedREST("/configuration/runservers/{runserverId}/tools/{toolId}/parameters",
                                                         urlParams = list(runserverId=runserverTool$runserverId,toolId=runserverTool$id))
        toolParameterContent <- httr::content(toolParameterResult)
        toolParameters <- mergeListToDataframe(toolParameterContent)

        # Check if toolParameters is empty or null
        if (is.null(toolParameters) || nrow(toolParameters) == 0) {
          # Use empty parameters list
          toolParameters <- data.frame(lovType = character(),
                                     name = character(),
                                     description = character(),
                                     value = character())
        } else {
          # Check if we have the expected columns for the join
          if ("parameterLovId" %in% names(toolParameters) && "id" %in% names(parameters)) {
            # Perform the join and select the needed columns
            joined <- dplyr::inner_join(parameters, toolParameters, by = c("id" = "parameterLovId"))

            # Check which columns exist in the joined result
            available_cols <- intersect(c("lovType", "name", "description", "value"), names(joined))
            if (length(available_cols) > 0) {
              toolParameters <- dplyr::select(joined, dplyr::all_of(available_cols))
            } else {
              # If expected columns don't exist, create empty dataframe
              toolParameters <- data.frame(lovType = character(),
                                         name = character(),
                                         description = character(),
                                         value = character())
            }
          } else {
            # If join columns don't exist, create empty dataframe
            toolParameters <- data.frame(lovType = character(),
                                       name = character(),
                                       description = character(),
                                       value = character())
          }
        }

        runserverTool$parameters <- list(toolParameters)
        ####gridArguments
        tryCatch( {
          if(!is.na(runserverTool$gridProvider)) {
            gridArgumentDefinitions <- loadGridArguments(runserverTool$gridProvider)
            gridArgumentResult<-authenticatedREST("/configuration/runservers/{runserverId}/tools/{toolId}/gridArguments",
                                                            urlParams = list(runserverId=runserverTool$runserverId,toolId=runserverTool$id))
            gridArguments <- mergeListToDataframe(httr::content(gridArgumentResult))

            # Only process if both dataframes have data
            if (!is.null(gridArguments) && nrow(gridArguments) > 0 &&
                !is.null(gridArgumentDefinitions) && nrow(gridArgumentDefinitions) > 0) {

              # Check if we have the expected columns for the join
              if ("definitionId" %in% names(gridArguments) && "id" %in% names(gridArgumentDefinitions)) {
                merged <- dplyr::inner_join(gridArgumentDefinitions, gridArguments, by = c("id" = "definitionId"))

                # Join with grid values if lovValueId exists
                if ("lovValueId" %in% names(merged) && !is.null(gridValues)) {
                  mergedValues <- dplyr::left_join(merged, gridValues, by = c("lovValueId" = "id"))
                } else {
                  mergedValues <- merged
                }

                # Add missing columns with NA
                if (!("textValue" %in% names(mergedValues))) {
                  mergedValues$textValue <- NA
                }
                if (!("text" %in% names(mergedValues))) {
                  mergedValues$text <- NA
                }

                # Create the value column
                mergedValues <- dplyr::mutate(mergedValues, value = dplyr::if_else(is.na(.data$textValue), .data$text, .data$textValue))

                # Select only columns that exist
                available_cols <- intersect(c("name", "value"), names(mergedValues))
                if (length(available_cols) > 0) {
                  gridArguments <- dplyr::select(mergedValues, dplyr::all_of(available_cols))
                  runserverTool$gridArguments <- list(gridArguments)
                }
              }
            }
          }
        }, error = function(e) {
          # Silent catch - some tools may not have grid arguments
        })

        return(runserverTool)
      })

      return(runServerTools)
    })
    runserverTools <- dplyr::mutate(runserverTools,fullName=paste(.data$categoryName,.data$toolName,.data$name,.data$label))
    toolInstanceEnv <- new.env()
    x <- byNotEmpty(runserverTools,function(runserverTool) {
      assign(x=runserverTool$fullName,value=runserverTool,envir =toolInstanceEnv )
    })
    assign(x = "toolInstances",value = toolInstanceEnv,envir = toolInstanceCache)
    return(toolInstanceEnv)

  }



}
