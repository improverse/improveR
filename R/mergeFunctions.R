mergeDataframeList <- function(dfList) {
  as.data.frame(do.call(plyr::rbind.fill,dfList),stringsAsFactors = FALSE)
}

mergeListToDataframe <- function(entryList) {
  dfs <- lapply(entryList,function(entry) {
    row <- as.data.frame(entry,stringsAsFactors = FALSE)
    if("targetEntityId" %in% names(row) && !grepl(pattern = ":",x=row$targetEntityId,fixed = T)) {
      row$targetEntityId <- paste0(repoPrefix(),row$targetEntityId)
    }
    if("targetEntityVersionId" %in% names(row) && !grepl(pattern = ":",x=row$targetEntityVersionId,fixed = T)) {
      row$targetEntityVersionId <- paste0(repoPrefix(),row$targetEntityVersionId)
    }
    if("entityId" %in% names(row) && !grepl(pattern = ":",x=row$entityId,fixed = T)) {
      row$entityId <- paste0(repoPrefix(),row$entityId)
    }
    if("entityVersionId" %in% names(row) && !grepl(pattern = ":",x=row$entityVersionId,fixed = T)) {
      row$entityVersionId <- paste0(repoPrefix(),row$entityVersionId)
    }
    return(row)
  })
  mergeDataframeList(dfs)
}

mergeNestedListToDataframe <- function (entryList)
{
  dfs <- lapply(entryList, function(row) {
    if (length(row)==0) {
      return("")
    }
    if (length(row)==1) {
      row <- row[[1]]
    }
    if (length(row)==0) {
      return("")
    }
    lsts <- lapply(row, function(column) {
      if(typeof(column)=="list") {
        return(column)
      }
      return(NA)
    })
    lsts <- lsts[sapply(lsts,is.list)]


    row <- lapply(row, function(column) {
      if(typeof(column)=="list") {
        return(NA)
      }
      return(column)
    })
    row <- row[!sapply(row,is.na)]

    if (length(row)==0) {
      return(
        mergeListToDataframe(lsts)
      )
    }

    row <- tidyr::as_tibble(row, stringsAsFactors = FALSE)
    if (length(lsts)>0) {
      lstsNames <- names(lsts)
      for (i in 1:length(lstsNames)) {
        lstName <- lstsNames[i]
        df <- mergeNestedListToDataframe(lsts[i])
        if (nrow(df)==0) {
          row[lstName]<-""
        } else {
          row[lstName]<-tidyr::nest(
            df
            ,data = tidyr::everything()
          )
        }
      }
    }
    if("targetEntityId" %in% names(row) && !grepl(pattern = ":",x=row$targetEntityId,fixed = T)) {
      row$targetEntityId <- paste0(repoPrefix(),row$targetEntityId)
    }
    if("targetEntityVersionId" %in% names(row) && !grepl(pattern = ":",x=row$targetEntityVersionId,fixed = T)) {
      row$targetEntityVersionId <- paste0(repoPrefix(),row$targetEntityVersionId)
    }
    if("entityId" %in% names(row) && !grepl(pattern = ":",x=row$entityId,fixed = T)) {
      row$entityId <- paste0(repoPrefix(),row$entityId)
    }
    if("entityVersionId" %in% names(row) && !grepl(pattern = ":",x=row$entityVersionId,fixed = T)) {
      row$entityVersionId <- paste0(repoPrefix(),row$entityVersionId)
    }
    return(row)
  })
  mergeDataframeList(dfs)
}
