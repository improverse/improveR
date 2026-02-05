#' Loads the Metadata of a Given Resource and Writes It as String Values Into a List
#'
#' @param ident path, resource or entity ID of the picture
#' @param from used for relative paths, by default pwd is used, which is initiated with the step that started improveR
#'
#' @export
getMetaDataMap <- function(ident,from=pwd()) {


  metaData <- loadMetaData(ident,from = from)
  if (is.null(metaData)) {
    return(NULL)
  }
  metaData <- metaData$data[[1]]

  env<-new.env()
  columnNames <- names(metaData)
  if (nrow(metaData)>0) {
    by(metaData,1:nrow(metaData),function(m) {
      if (!is.null(m$descriptorName)) {
        if (m$descriptorType=="TEXT" && ("textValue" %in% columnNames) && !is.na(m$textValue) && m$textValue!="") {
          assign(toString(m$descriptorName),toString(m$textValue),env)
        } else if (m$descriptorType=="LOV" && ("lovText" %in% columnNames) && !is.na(m$lovText) && m$lovText!="") {
          assign(toString(m$descriptorName),toString(m$lovText),env)
        }
        else if (m$descriptorType=="NUMBER" && ("numValue" %in% columnNames) && !is.na(m$numValue) && m$numValue!="") {
          assign(toString(m$descriptorName),toString(m$numValue),env)
        }
        else if (m$descriptorType=="DATE" && ("dateValue" %in% columnNames) && !is.na(m$dateValue) && m$dateValue!="") {
          assign(toString(m$descriptorName),convertImproveTimestampToPosix(as.numeric(m$dateValue)),env)
        }
        else if (m$descriptorType=="URL" && ("textValue" %in% columnNames) && !is.na(m$textValue) && m$textValue!="") {
          assign(toString(m$descriptorName),toString(m$textValue),env)
        }
      }
    })
  }

  return(as.list(env))
}

#' Gets a Metadata Data Frame
#'
#' @param ident path, resource or entity ID of the picture
#' @param from used for relative paths, by default pwd is used, which is initiated with the step that started improveR
#' @references ics1141
#' @export
getMetaData <- function(ident,from=pwd()) {
  metaData <- loadMetaData(ident,from = from)
  if (is.null(metaData)) {
    return(NULL)
  }
  metaData <- metaData$data[[1]]
  columnNames <- names(metaData)
  metaData<-by(metaData,1:nrow(metaData),function(m) {
    if (!is.null(m$descriptorName)) {
      if (m$descriptorType=="TEXT" && ("textValue" %in% columnNames) && !is.na(m$textValue) && m$textValue!="") {
        m$value <- toString(m$textValue)
      } else if (m$descriptorType=="LOV" && ("lovText" %in% columnNames) && !is.na(m$lovText) && m$lovText!="") {
        m$value <- toString(m$lovText)
      }
      else if (m$descriptorType=="NUMBER" && ("numValue" %in% columnNames) && !is.na(m$numValue) && m$numValue!="") {
        m$value <- toString(m$numValue)
      }
      else if (m$descriptorType=="DATE" && ("dateValue" %in% columnNames) && !is.na(m$dateValue) && m$dateValue!="")  {
        m$value <- toString(convertImproveTimestampToPosix(as.numeric(m$dateValue)))
        #m$value <- as.numeric(m$dateValue)
      }
      else if (m$descriptorType=="URL" && ("textValue" %in% columnNames) && !is.na(m$textValue) && m$textValue!="") {
        m$value <- toString(m$textValue)
      }
    }
    return(m)
  })
  metaDf <- mergeListToDataframe(metaData)
  if (is.null(metaDf$resourceCharacterId)) {
    metaDf$resourceCharacterId<-""
  }
  if (is.null(metaDf$categoryId)) {
    metaDf$categoryId<-""
  }
  if (is.null(metaDf$categoryName)) {
    metaDf$categoryName<-""
  }
  if (is.null(metaDf$lovId)) {
    metaDf$lovId<-""
  }
  if (is.null(metaDf$value)) {
    metaDf$value<-""
  }

  col_order <- c("descriptorName", "value", "descriptorType",
                 "resourceId","descriptorId","scope","metadataId","metadataHistoryId","deleted","inheritToChilds","inheritedFromParent","mandatory","resourceCharacterId","categoryId","categoryName","lovId","value")
  metaDf <- metaDf[, col_order]
  return (metaDf)
}
