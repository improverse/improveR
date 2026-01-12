#' Search Resources Using Elasticsearch Query
#'
#' Executes a search query against the improve platform's Elasticsearch index to find
#' resources matching the specified criteria. Uses the same query syntax as the improve
#' web UI search interface, supporting field-based searches, wildcards, and logical operators.
#'
#' @param query Character. Search query using improve's query grammar. Examples:
#'   \itemize{
#'     \item \code{"name='analysis'"} - exact name match
#'     \item \code{"name='*analysis*'"} - name contains "analysis"
#'     \item \code{"nodeType='Step' AND runStatus='FINISHED'"} - combined criteria
#'     \item \code{"description='*population*'"} - search in description field
#'   }
#' @param fetchSize Numeric. Maximum number of results to return. Defaults to 100.
#'   Use for pagination or limiting large result sets.
#' @param fetchOffset Numeric. Number of results to skip before returning matches.
#'   Defaults to 0. Use with \code{fetchSize} to implement pagination.
#'
#' @return A data frame containing the matched resources with their metadata, or
#'   \code{NULL} if no resources match the query or if the query contains errors.
#'   The data frame includes standard resource fields like \code{resourceId}, \code{entityId},
#'   \code{name}, \code{path}, \code{nodeType}, and any additional fields returned by the
#'   search (depending on the query).
#'
#' @details
#' The function submits the query to the improve platform's Elasticsearch backend,
#' which indexes resource metadata for fast searching. Search results include the
#' \code{resourceId} for each match, which is then used to load full resource details.
#'
#' Query syntax supports:
#' \itemize{
#'   \item Field searches: \code{fieldName='value'}
#'   \item Wildcards: \code{fieldName='*partial*'}
#'   \item Logical operators: \code{AND}, \code{OR}
#'   \item Date fields: use ISO format for date comparisons
#'   \item Path-based searches: \code{path='/Some/Folder/*'}
#' }
#'
#' If the query contains errors, a warning is logged with the error message from the
#' server, and \code{NULL} is returned.
#'
#' For searches within a specific folder hierarchy, use \code{\link{queryFolder}} which
#' automatically restricts results to a folder and its descendants.
#'
#' @seealso
#' \code{\link{queryFolder}} for folder-scoped searches,
#' \code{\link{loadResource}} to load resources by known identifiers
#'
#' @examples
#' \dontrun{
#' # Find all finished steps
#' query("nodeType='Step' AND runStatus='FINISHED'")
#'
#' # Search for resources by name pattern
#' query("name='*baseline*'")
#'
#' # Paginate through large result sets
#' page1 <- query("nodeType='File'", fetchSize = 50, fetchOffset = 0)
#' page2 <- query("nodeType='File'", fetchSize = 50, fetchOffset = 50)
#'
#' # Find resources in a specific path
#' query("path='/Modeling/PopPK/*'")
#' }
#'
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

#' Search Resources Within a Folder Using Elasticsearch Query
#'
#' Executes a search query limited to resources within a specific folder and its descendants.
#' This is a convenience wrapper around \code{\link{query}} that automatically restricts
#' the search scope to a folder hierarchy, using the same query syntax as the improve web UI.
#'
#' @param queryString Character. Search query using improve's query grammar. The query is
#'   automatically combined with a path restriction to limit results to the specified folder.
#'   Use the same syntax as in \code{\link{query}}.
#' @param fetchSize Numeric. Maximum number of results to return. Defaults to 100.
#' @param fetchOffset Numeric. Number of results to skip before returning matches.
#'   Defaults to 0. Use with \code{fetchSize} for pagination.
#' @param ident Identifier of the folder to search within. Can be a path, resource id,
#'   or entity id. Only resources in this folder or its subfolders will be returned.
#' @param from Starting point for relative path resolution. Defaults to current working
#'   directory from \code{pwd()}. Only used if \code{ident} is a relative path.
#'
#' @return A data frame containing the matched resources within the folder hierarchy,
#'   or \code{NULL} if no resources match, the query contains errors, the folder doesn't
#'   exist, or multiple folders are provided (only one folder allowed).
#'
#' @details
#' The function loads the specified folder, constructs a path-based filter, and combines
#' it with your query string. The resulting query searches for resources where the path
#' equals the folder path or starts with the folder path followed by \code{/*}.
#'
#' This is particularly useful for:
#' \itemize{
#'   \item Searching within a specific project or workflow folder
#'   \item Finding resources in a folder hierarchy without knowing exact paths
#'   \item Limiting search scope to improve performance and relevance
#' }
#'
#' The function will log a warning and return \code{NULL} if multiple folders are
#' provided in \code{ident}, as only single-folder searches are supported.
#'
#' @seealso
#' \code{\link{query}} for repository-wide searches,
#' \code{\link{loadChildResources}} to list immediate children of a folder
#'
#' @examples
#' \dontrun{
#' # Find all CSV files in a modeling folder
#' queryFolder(
#'   ident = "/Modeling/PopPK",
#'   queryString = "name='*.csv'"
#' )
#'
#' # Find finished steps in a workflow
#' queryFolder(
#'   ident = "/Workflows/Analysis",
#'   queryString = "nodeType='Step' AND runStatus='FINISHED'"
#' )
#'
#' # Search with pagination
#' queryFolder(
#'   ident = "/Data",
#'   queryString = "nodeType='File'",
#'   fetchSize = 20,
#'   fetchOffset = 0
#' )
#' }
#'
#' @export

queryFolder <- function(queryString, fetchSize=100,fetchOffset=0,ident,from=pwd()) {
  resource <- loadResource(ident,from)
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
