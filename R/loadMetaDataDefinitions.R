metadataDefinitionsCacheList <- list(
  metadataDefinitionsCache="scope"
)


actualLoadMetaDataDefinitions <- function(scope="Improve Client"){

  scope <- utils::URLencode(scope,reserved = T)
  result <- authenticatedREST('/configuration/metadata/{scope}/definitions',
                                            urlParams = list(scope=scope
                                            ),
                                            restType = "GET")
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

#' loads all meta data definitions for a scope, the default scope is "Improve Client"
#' @param scope the metadata scope
#' @references ics1137
#' @export
loadMetaDataDefinitions <- function(scope="Improve Client") {
  metaDataDefinitions <- getFromCache(scope,actualLoadMetaDataDefinitions,metadataDefinitionsCacheList,NULL)
  return(metaDataDefinitions)
}


#' unloadMetaDataDefinitions
#' @param scope the metadata scope
#' @references ics1137
#' @export
unloadMetaDataDefinitions <- function(scope="Improve Client") {
  loadMetaDataDefinitions(scope)
  removeFromCache(scope,"",metadataDefinitionsCacheList)
}

#' updateMetaDataDefinitions
#' @param scope the metadata scope
#' @references ics1137
#' @export
updateMetaDataDefinitions <- function(scope="Improve Client") {
  unloadMetaDataDefinitions(scope)
  res <- loadMetaDataDefinitions(scope)
  return(res)
}

#' loads one meta data definitions by name for a scope, the default scope is "Improve Client"
#' @param scope the metadata scope
#' @param name, name of the meta data definition
#' @references ics1137
#' @export
loadMetaDataDefinition<- function(name,scope="Improve Client") {
  definitions <- loadMetaDataDefinitions(scope)
  definition <- definitions[definitions$name==name,]
  return(definition)
}

#' loads one meta data definition picklist values by name for a scope, the default scope is "Improve Client"
#' @param scope the metadata scope
#' @param name, name of the meta data definition
#' @references ics1137
#' @export
loadMetaDataDefinitionPickList<- function(name,scope="Improve Client") {
  definitions <- loadMetaDataDefinitions(scope)
  definition <- definitions[definitions$name==name,]
  return(definition$categoryValues[[1]])
}


