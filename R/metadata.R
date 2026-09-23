




createMetaDataBody <- function(resourceId,descriptorName, value,scope="Improve Client"){
  improveEditable()
  scope <- utils::URLencode(scope,reserved = T)
  mdData <- loadMetaDataDefinition(descriptorName,scope=scope)
  picklist <- NULL
  if ("categoryValues" %in% names(mdData) && length(mdData$categoryValues)>0) {
    picklist <- mdData$categoryValues[[1]]
  }
  metaDataBody <- list(
    descriptorType=as.character(mdData$metadataType),
    #metadataId=mdData$id,
    resourceId=resourceId,
    descriptorId=as.character(mdData$id),
    scope=scope,
    deleted=FALSE,
    inheritedFromParent=FALSE,
    mandatory=FALSE
  )
  if (metaDataBody$descriptorType=="TEXT") {
    metaDataBody$textValue=value
  } else if (metaDataBody$descriptorType=="NUMBER") {
    metaDataBody$numValue=value
  } else if (metaDataBody$descriptorType=="URL") {
    metaDataBody$textValue=value
  } else if (metaDataBody$descriptorType=="DATE") {
    metaDataBody$dateValue=as.numeric(format(value,"%s"))*1000
  } else if (metaDataBody$descriptorType=="LOV") {
    metaDataBody$lovId=as.character(picklist[picklist$text==value,]$id)
  }
  if (mdData$strategy=="Inherit") {
    metaDataBody$inheritToChilds=TRUE
  } else {
    metaDataBody$inheritToChilds=FALSE
  }
  return(metaDataBody)
}

singleAddMetaDate <- function(ident,descriptorName, value,scope = "Improve Client") {
  entryList <- list(
    list(descriptorName=descriptorName,
         value=value)
  )
  return(addBulkMetaDate(ident,entryList,scope=scope))
}

#' Adds Metadata Value To A Resource, Always Adds, Never Updates
#' @param ident the resource id of the resource this metadata value is attached to
#' @param descriptorName this descriptorName has to exist, check in preferences
#' @param value the value has to match the type of the descriptor. For dates, posix date has to be used, for LOVs the text has to exist
#' @param scope the scope for metadata, default "Improve Client"
#' @references ics1137
#' @returns The refreshed metadata of the resource, as [refreshMetaData()] returns it: a
#'   data frame with the metadata table nested in `$data`. `NULL` when `ident` resolves to
#'   no resource, or is an empty data frame or an empty vector. For several idents the
#'   refreshed metadata of all of them, merged into one data frame. Note that the value
#'   reports the state **after** the write, not whether the write itself succeeded.
#' @export
addMetaDate <- function(ident,descriptorName, value,scope = "Improve Client") {
  return(
    multiplexResourceFunction(func=singleAddMetaDate,
                              multiArgument = ident,
                              descriptorName=descriptorName,
                              value=value,
                              scope=scope)
  )
}

singleAddBulkMetaDate <- function(ident,descriptorNameValueList, scope="Improve Client") {
  resource <- loadResource(ident)

  dataList <- lapply(descriptorNameValueList,function(nameListEntry) {
    mdBody <- createMetaDataBody(resource$resourceId,nameListEntry$descriptorName, nameListEntry$value,scope = scope)
    return(mdBody)
  })

  result <- authenticatedREST('resources/{resourceId}/metadata/bulk',
                                            urlParams = list(
                                              resourceId=resource$resourceId
                                            ),
                                            data = dataList,
                                            restType = "POST")

  metadata <- refreshMetaData(ident)
  return(metadata)
}

#' Adds Metadata Value To A Resource, Always Adds, Never Updates
#' @param ident the resource id of the resource this metadata value is attached to
#' @param descriptorNameValueList a list of lists, containing descriptorName and value: this descriptorName has to exist, check in preferences, the value has to match the type of the descriptor. For dates, posix date has to be used, for LOVs the text has to exist
#' @param scope the scope for metadata, default "Improve Client"
#' @references ics1137
#' @returns The refreshed metadata of the resource, as [refreshMetaData()] returns it: a
#'   data frame with the metadata table nested in `$data`. `NULL` when `ident` resolves to
#'   no resource, or is an empty data frame or an empty vector. For several idents the
#'   refreshed metadata of all of them, merged into one data frame. Note that the value
#'   reports the state **after** the write, not whether the write itself succeeded.
#' @export
addBulkMetaDate <- function(ident,descriptorNameValueList,scope="Improve Client") {
  return(
    multiplexResourceFunction(func=singleAddBulkMetaDate,
                              multiArgument = ident,
                              descriptorNameValueList=descriptorNameValueList,
                              scope=scope)
  )
}


singleDeleteMetaDate <- function(ident,descriptorName) {
  resource <- loadResource(ident)
  for  (i in 1:length(descriptorName)) {
    result <- authenticatedREST('resources/{resourceId}/metadata/{name}',
                                              urlParams = list(
                                                resourceId=resource$resourceId,
                                                name=descriptorName[i]
                                              ),
                                              restType = "DELETE")
  }
  metadata <- refreshMetaData(ident)
  return(metadata)
}

#' Deletes Metadata From A Resource
#' @param ident the resource id of the resource this metadata value is attached to
#' @param descriptorName this descriptorName has to exist, check in preferences
#' @references ics1137
#' @returns The refreshed metadata of the resource, as [refreshMetaData()] returns it: a
#'   data frame with the metadata table nested in `$data`. `NULL` when `ident` resolves to
#'   no resource, or is an empty data frame or an empty vector. For several idents the
#'   refreshed metadata of all of them, merged into one data frame. Note that the value
#'   reports the state **after** the write, not whether the write itself succeeded.
#' @export
deleteMetaDate <- function(ident,descriptorName) {
  return(
    multiplexResourceFunction(func=singleDeleteMetaDate,
                              multiArgument = ident,
                              descriptorName=descriptorName)
  )
}


singleUpdateMetaDate <- function(ident,descriptorName, value) {
  resource <- loadResource(ident)
  metadata <- loadMetaData(ident)$data[[1]]
  metadata <- metadata[metadata$descriptorName==descriptorName,]
  if (nrow(metadata)==0 || nrow(metadata)>1) {
    log_warn(paste0(
      "0 or more than one meta date found for descriptor ",
      descriptorName,
      " on resource ",
      ident)
    )
    return(loadMetaData(ident))
  }

  scope <- metadata$scope
  mdBody <- createMetaDataBody(resource$resourceId,descriptorName, value,scope=scope)
  mdBody$metadataHistoryId<-metadata$metadataHistoryId
  mdBody$metadataId<-metadata$metadataId


  result <- authenticatedREST('resources/{resourceId}/metadata/{descriptor}',
                                            urlParams = list(
                                              resourceId=resource$resourceId,
                                              descriptor=descriptorName
                                            ),
                                            data = mdBody,
                                            restType = "PUT")

  metadata <- refreshMetaData(ident)
  return(metadata)
}

#' Updates Metadata Value To A Resource
#' @param ident the resource this metadata value is attached to
#' @param descriptorName name of the metadate
#' @param value the value has to match the type of the descriptor. For dates, posix date has to be used, for LOVs the text has to exist
#' @references ics1137
#' @returns The refreshed metadata of the resource, as [refreshMetaData()] returns it: a
#'   data frame with the metadata table nested in `$data`. `NULL` when `ident` resolves to
#'   no resource, or is an empty data frame or an empty vector. For several idents the
#'   refreshed metadata of all of them, merged into one data frame. Note that the value
#'   reports the state **after** the write, not whether the write itself succeeded.
#' @export
updateMetaDate <- function(ident,descriptorName, value) {
  return(
    multiplexResourceFunction(func=singleUpdateMetaDate,
                              multiArgument = ident,
                              descriptorName=descriptorName,
                              value=value)
  )
}

singleUpdateMetaDateById <- function(ident,metadataId, value) {
  resource <- loadResource(ident)
  metadata <- loadMetaData(ident)$data[[1]]
  descriptorName <- metadata[metadata$metadataId==metadataId,]$descriptorName
  scope <- metadata[metadata$metadataId==metadataId,]$scope
  mdBody <- createMetaDataBody(resource$resourceId,descriptorName, value,scope=scope)
  metadata<-loadMetaData(ident)
  metadata<-metadata[metadata$metadataId==metadataId,]
  mdBody$metadataHistoryId<-metadata$metadataHistoryId
  mdBody$metadataId<-metadata$metadataId


  result <- authenticatedREST('resources/{resourceId}/metadata/{descriptor}',
                                            urlParams = list(
                                              resourceId=resource$resourceId,
                                              descriptor=descriptorName
                                            ),
                                            data = mdBody,
                                            restType = "PUT")

  metadata <- refreshMetaData(ident)
  return(metadata)
}

#' Updates Metadata Value To A Resource
#' @param ident the resource this metadata value is attached to
#' @param metadataId the meta data update
#' @param value the value has to match the type of the descriptor. For dates, posix date has to be used, for LOVs the text has to exist
#' @references ics1137
#' @returns The refreshed metadata of the resource, as [refreshMetaData()] returns it: a
#'   data frame with the metadata table nested in `$data`. `NULL` when `ident` resolves to
#'   no resource, or is an empty data frame or an empty vector. For several idents the
#'   refreshed metadata of all of them, merged into one data frame. Note that the value
#'   reports the state **after** the write, not whether the write itself succeeded.
#' @export
updateMetaDateById <- function(ident,metadataId, value) {
  return(
    multiplexResourceFunction(func=singleUpdateMetaDateById,
                              multiArgument = ident,
                              metadataId=metadataId,
                              value=value)
  )
}
