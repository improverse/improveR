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
#' @export
loadMetaDataDefinitions <- function(scope="Improve Client") {
  metaDataDefinitions <- getFromCache(scope,actualLoadMetaDataDefinitions,metadataDefinitionsCacheList,NULL)
  return(metaDataDefinitions)
}


#' Unload Meta Data Definitions
#' @param scope the metadata scope
#' @references ics1137
#' @export
unloadMetaDataDefinitions <- function(scope="Improve Client") {
  removeFromCache(scope,"",metadataDefinitionsCacheList)
}

#' Refresh Meta Data Definitions
#' @param scope the metadata scope
#' @param ... For backwards compatibility with the deprecated `update*` alias; not used by `refresh*` itself.
#' @references ics1137
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
#' @param name, name of the meta data definition
#' @references ics1137
#' @export
loadMetaDataDefinition<- function(name,scope="Improve Client") {
  definitions <- loadMetaDataDefinitions(scope)
  definition <- definitions[definitions$name==name,]
  return(definition)
}

#' Loads One Meta Data Definition Picklist Values By Name For A Scope, The Default Scope Is "Improve Client"
#' @param scope the metadata scope
#' @param name, name of the meta data definition
#' @references ics1137
#' @export
loadMetaDataDefinitionPickList<- function(name,scope="Improve Client") {
  definitions <- loadMetaDataDefinitions(scope)
  definition <- definitions[definitions$name==name,]
  return(definition$categoryValues[[1]])
}


