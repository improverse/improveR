metadataDefinitionsCacheList <- list(
  metadataDefinitionsCache="scope"
)


actualLoadMetaDataDefinitions <- function(scope="Improve Client"){

  scope <- utils::URLencode(scope,reserved = T)
  result <- authenticatedREST('/configuration/metadata/{scope}/definitions',
                                            urlParams = list(scope=scope
                                            ),
                                            restType = "GET")
  if (is.null(result)) return(NULL)
  definitions <- httr::content(result)
  definitions <- lapply(definitions, function(definition) {
    if ("category" %in% names(definition)) {
      category <- definition$category
      definition$category <- ""
      df <- as.data.frame(definition,stringsAsFactors = F)
      df$categoryName <- category$name
      df$categoryValues <- list(plyr::rbind.fill(lapply(category$values,as.data.frame)))
      return(df)
    }
    df <- as.data.frame(definition,stringsAsFactors = F)
    return(df)
  })
  definitions <- plyr::rbind.fill(definitions)
  return(definitions)
}

#' Loads All Meta Data Definitions For A Scope, The Default Scope Is "Improve Client"
#' @param scope the metadata scope
#' @references ics1137
#' @returns A data frame of every metadata definition in the scope, one row each, as the
#'   server returns them. Cached: the second call for the same scope does not reach the
#'   server.
#' @export
loadMetaDataDefinitions <- function(scope="Improve Client") {
  metaDataDefinitions <- getFromCache(scope,actualLoadMetaDataDefinitions,metadataDefinitionsCacheList,NULL)
  return(metaDataDefinitions)
}


#' Unload Meta Data Definitions
#' @param scope the metadata scope
#' @references ics1137
#' @returns No meaningful value - called for its side effect of dropping the metadata
#'   definitions of the scope from the cache. They are repository-wide configuration, so
#'   this affects every later call in the session, not only the caller's.
#' @export
unloadMetaDataDefinitions <- function(scope="Improve Client") {
  removeFromCache(scope,"",metadataDefinitionsCacheList)
}

#' Refresh Meta Data Definitions
#' @param scope the metadata scope
#' @param ... For backwards compatibility with the deprecated `update*` alias; not used by `refresh*` itself.
#' @references ics1137
#' @returns The freshly read metadata definitions for the scope, in the same shape as
#'   [loadMetaDataDefinitions()]. The definitions are repository-wide configuration, so
#'   this affects every later call in the session, not only the caller's.
#' @export
refreshMetaDataDefinitions <- function(scope="Improve Client") {
  unloadMetaDataDefinitions(scope)
  res <- loadMetaDataDefinitions(scope)
  return(res)
}

#' @rdname refreshMetaDataDefinitions
#' @export
updateMetaDataDefinitions <- function(...) {
  .Deprecated("refreshMetaDataDefinitions")
  refreshMetaDataDefinitions(...)
}

#' Loads One Meta Data Definitions By Name For A Scope, The Default Scope Is "Improve Client"
#' @param scope the metadata scope
#' @param name name of the meta data definition
#' @references ics1137
#' @returns The single row of [loadMetaDataDefinitions()] whose `name` matches, as a data
#'   frame. A name that does not exist in the scope yields a data frame with zero rows, not
#'   `NULL`.
#' @export
loadMetaDataDefinition<- function(name,scope="Improve Client") {
  definitions <- loadMetaDataDefinitions(scope)
  definition <- definitions[definitions$name==name,]
  return(definition)
}

#' Loads One Meta Data Definition Picklist Values By Name For A Scope, The Default Scope Is "Improve Client"
#' @param scope the metadata scope
#' @param name name of the meta data definition
#' @references ics1137
#' @returns The pick list values of the definition - the `categoryValues` entry of the
#'   matching row. Only meaningful for a definition of type `LOV`; for a name that does not
#'   exist in the scope the call fails, because there is no row to take the values from.
#' @export
loadMetaDataDefinitionPickList<- function(name,scope="Improve Client") {
  definitions <- loadMetaDataDefinitions(scope)
  definition <- definitions[definitions$name==name,]
  return(definition$categoryValues[[1]])
}


