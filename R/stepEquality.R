
#' existsInTargetTree
#'
#' @param handle check if a step with the same configuration already exists in the tree
#' @references ics1228
#' @export

existsInTargetTree <- function(handle) {
  prepStep <- retrieveStep(handle)
  prepStep <- addDefaultProcessParams(prepStep)
  tree <- prepStep$treeIdent
  compareWorkflow <- handlesFromTree(tree)
  compareWorkflow <- retrieveWorkflow(compareWorkflow)
  if (nrow(compareWorkflow)==0) {
    return(NULL)
  }
  filteredWorkflow <- compareWorkflow %>%
    filterOptionalField(prepStep,"runserverName") %>%
    filterOptionalField(prepStep,"toolName") %>%
    filterOptionalField(prepStep,"runserverToolName") %>%
    filterOptionalField(prepStep,"rationale") %>%
    filterOptionalField(prepStep,"description") %>%
    filterOptionalField(prepStep,"toolArgs") %>%
    filterOptionalField(prepStep,"toolDeletePatterns") %>%
    filterOptionalField(prepStep,"toolStreamablePatterns")
  if (nrow(filteredWorkflow)==0) {
    return(NULL)
  }
  #compare Files
  prepRemote <- NULL
  prepLocal <- NULL
  fileNum<-0
  if (!is.null(prepStep$remoteFiles)) {
    prepRemote <- prepStep$remoteFiles[[1]]
    fileNum <- fileNum+nrow(prepRemote)
  }
  if (!is.null(prepStep$localFiles)) {
    prepLocal <- prepStep$localFiles[[1]]
    fileNum <- fileNum+nrow(prepLocal)
  }


  #compare files

  filteredWorkflow <- byNotEmptyAsDf(filteredWorkflow,function(compareStep) {
    files <- compareStep$remoteFiles
    if (!is.null(files)) {
      files <- files[[1]]
      if (nrow(files)==fileNum) {
        #remote
        resultRemote<-T
        if (!is.null(prepRemote)) {
          resultRemote <- all(
            byNotEmpty(prepRemote,function(prepFile) {
              if ("ident" %in% names(prepFile) && !is.na(prepFile$ident)) {
                if (!("name" %in% names(prepFile)) || is.na(prepFile$name)) {
                  name <- loadResource(prepFile$ident)$name
                  prepFile$name <- name
                }
                foundFiles <- files %>%
                  dplyr::filter(.data$name==prepFile$name) %>%
                  dplyr::filter(.data$ident==prepFile$ident)  %>%
                  dplyr::filter(.data$asLink==prepFile$asLink) %>%
                  filterOptionalField(prepFile,"variableName")
                return(nrow(foundFiles)==1)
              } else if ("sourceHandle" %in% names(prepFile) && !is.na(prepFile$sourceHandle)) {
                #referenceHandle <- retrieveStep(prepFile$sourceHandle)
                referenceInventory <- getStepInventory(prepFile$sourceHandle,recurse = T)$data[[1]]
                referenceFile <- referenceInventory %>%
                  dplyr::filter(.data$inventoryPath==prepFile$sourceName)
                if (!("name" %in% names(prepFile)) || is.na(prepFile$name)) {
                  prepFile$name <- referenceFile$name
                }
                foundFiles <- files %>%
                  dplyr::filter(.data$name==prepFile$name) %>%
                  dplyr::filter(.data$ident==referenceFile$entityId)  %>%
                  filterOptionalField(prepFile,"asLink") %>%
                  filterOptionalField(prepFile,"variableName")
                return(nrow(foundFiles)==1)
              }
              return(F)
            })
          )
        }
        resultLocal<-T
        if (!is.null(prepLocal)) {
          files <- byNotEmptyAsDf(files,function(fi) {
            f <- loadResource(fi$ident)
            fi$fileHash <- f$fileHash
            return(fi)
          })
          resultLocal <- all(
            byNotEmpty(prepLocal,function(prepFile) {
              if (!("name" %in% names(prepFile)) || is.na(prepFile$name)) {
                name <- basename(prepFile$path)
                prepFile$name <- name
              }
              prepFile$asLink=F
              realFile <- file(prepFile$path)
              sha <- openssl::sha256(realFile)
              sha <- toupper(as.character(sha))
              foundFiles <- files %>%
                dplyr::filter(.data$name==prepFile$name) %>%
                dplyr::filter(.data$fileHash==sha)  %>%
                filterOptionalField(prepFile,"asLink") %>%
                filterOptionalField(prepFile,"variableName")
              return(nrow(foundFiles)==1)
            })

          )
        }
        if (resultLocal && resultRemote) {
          return(compareStep)
        }
      }
    }
    return(NULL)
  })

  #finished and up2date
  filteredWorkflow <- byNotEmptyAsDf(filteredWorkflow,function(filterStep) {
    stepResource <- getStepWithoutCache(filterStep$handle)
    if (stepResource$runStatus=="FINISHED") {
      if (!isStepOutdatedOrChanged(filterStep)) {
        return(filterStep)
      }
    }
  })

  # same grid arguments

  prepArguments <- prepStep$gridArguments
  filteredWorkflow <- byNotEmptyAsDf(filteredWorkflow,function(filterStep) {
    compareArguments <- filterStep$gridArguments
    prepHas <- !(is.na(prepArguments) || is.null(prepArguments) || is.null(prepArguments[[1]]))
    compHas <- !(is.na(compareArguments) || is.null(compareArguments) || is.null(compareArguments[[1]]))
    if (!prepHas && !compHas) {
      return(filterStep)
    }
    if (prepHas && compHas) {
      prepArguments <- prepArguments[[1]][order(prepArguments[[1]]$argumentName),] %>% dplyr::select("argumentName","argumentValue")
      compareArguments <- compareArguments[[1]][order(compareArguments[[1]]$argumentName),]%>% dplyr::select("argumentName","argumentValue")
      if (nrow(prepArguments)==nrow(compareArguments) && all(prepArguments==compareArguments)) {
        return(filterStep)
      }
    }
  })

  if (!is.null(filteredWorkflow) && nrow(filteredWorkflow)>0) {
    return(utils::head(filteredWorkflow,1))
  }
  return(NULL)
}


addDefaultProcessParams <- function(prepStep) {
  runserver <- loadRunserver(prepStep$runserverName)
  tool <- loadToolForRunserver(runserver$id,toolInstanceName = prepStep$runserverToolName)

  result <- mergeListToDataframe(
    httr::content(
      authenticatedREST("/configuration/runservers/{runserverId}/tools/{toolId}/parameters/",
                                      urlParams = list(runserverId=tool$runserverId,toolId=tool$id))
    )
  )

  paramLovs <- mergeListToDataframe(
    httr::content(
      authenticatedREST("/configuration/parameterLov")
    )
  )

  result$id<-result$parameterLovId
  result <- merge(result,paramLovs,by="id")
  if (!("toolArgs" %in% names(prepStep)) || is.na(prepStep$toolArgs)) {
    prepStep$toolArgs <- result %>% dplyr::filter(.data$name=="Tool Arguments")%>%dplyr::pull(.data$value)%>%as.character()
  }
  if (!("toolDeletePatterns" %in% names(prepStep)) || is.na(prepStep$toolDeletePatterns)) {
    prepStep$toolDeletePatterns <- result %>% dplyr::filter(.data$name=="Delete Patterns")%>%dplyr::pull(.data$value)%>%as.character()
  }
  if (!("toolStreamablePatterns" %in% names(prepStep)) || is.na(prepStep$toolStreamablePatterns)) {
    prepStep$toolStreamablePatterns <- result %>% dplyr::filter(.data$name=="Streamable File Patterns")%>%dplyr::pull(.data$value)%>%as.character()
  }
  return(prepStep)
}

filterOptionalField <- function(filteredDf,compareDf,field) {
  if (!(field %in% names(compareDf)) && !(field %in% names(filteredDf))) {
    return(filteredDf)
  }
  if ((field %in% names(compareDf)) && (field %in% names(filteredDf))) {
    if (is.na(compareDf[[field]])) {
      return(dplyr::filter(filteredDf,is.na(get({{field}}))))
    } else {
      return(dplyr::filter(filteredDf,get({{field}}) == compareDf[[field]]))
    }
  } else if (!(field %in% names(compareDf)) && (field %in% names(filteredDf))){
    return(dplyr::filter(filteredDf,is.na(get({{field}}))))
  } else {
    return(data.frame())
  }
}
