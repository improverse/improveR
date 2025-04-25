#' performs this query against the elastic search
#'
#' @param query query with the same grammar as in the UI
#' @param fetchSize maximum number of returned results
#' @param fetchOffset offset of the results, enables paging
#' @references ics1143
#' @export

query <- function(query, fetchSize=100,fetchOffset=0) {
  improveConnected()
  restURL <- "search/local"
  options <- list(fetchOffset=fetchOffset,
                  fetchSize=fetchSize)
  queryBody <- list(query=query,options=options)

  result <- httr::content(authenticatedREST(url = "/search/local",
                                            data = queryBody, restType = "POST"
  ))
  if (length(result$errors)>0) {
    log_warn("Error in query: ",result$errors[[1]])
  }
  entityIdPrefixString <- repoPrefix()

  resultList <- result$entries
  dfs <- lapply(resultList, function(item) {
    item <- item
    df <- data.frame(resourceId=item$resourceId)
    for (i in 1:length(item$resultFields)){
      subItem <- item$resultFields[i][[1]]
      thisval <-subItem$textValue
      if (is.null(subItem$textValue)) {
        thisval <-subItem$dateValue
      }
      name <- strsplit(subItem$name,"\\.")[[1]][1]
      if (name=="entityId" | name=="entityVersionId") {
        thisval <- paste(entityIdPrefixString,thisval)
      }
      df[name]<-thisval
    }
    return(df)
  })

  if (length(dfs)>0) {
    df <- loadResource(as.character(mergeListToDataframe(dfs)$resourceId))
    return(df)
  } else {
    return(NULL)
  }
}

#' performs this query against the elastic search within one folder
#'
#' @param ident path or id of a resource to start search from
#' @param fromForRelativePathes root for a relative path per default pwd
#' @param queryString query with the same grammar as in the UI
#' @param fetchSize maximum number of returned results
#' @param fetchOffset offset of the results, enables paging
#'
#' @export

queryFolder <- function(queryString, fetchSize=100,fetchOffset=0,ident,fromForRelativePathes=improveRcore::pwd()) {
  resource <- loadResource(ident,fromForRelativePathes)
  if (is.null(resource)) {
    return(NULL)
  }
  if (nrow(resource)>1) {
    logging::logwarn("queryFolder only allowed for one folder")
    return(NULL)
  }
  queryPrefix <- paste0("(path='",
                        resource$path,
                        "' OR path='",
                        resource$path,
                        "/*') AND ")
  queryString <- paste0(queryPrefix,queryString)
  return(query(queryString,fetchSize=fetchSize,fetchOffset=fetchOffset))
}
