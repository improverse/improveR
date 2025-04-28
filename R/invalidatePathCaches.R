invalidatePathCaches <- function(fromPath,deleteLinkedFiles=F) {
  if (!endsWith(fromPath,"/")) {
    fromPath <- paste0(fromPath,"/")
  }
  caches <- names(cacheEnv)
  pathCaches <- caches[grepl("PathCache",caches)]
  for (i in 1:length(pathCaches)) {
    cacheName <- pathCaches[i]
    cache <- cacheEnv[[cacheName]]
    entryNames <- names(cache)
    entryNames <- entryNames[startsWith(entryNames,fromPath)]
    ids <- lapply(entryNames,function(entryName) {
      entry <- cache[[entryName]]
      return(list(
        resourceId=entry$resourceId,
        entityId=entry$entityId,
        entityVersionId=entry$entityVersionId
      ))
    })

    idDf <- mergeListToDataframe(ids)
    rm(list=entryNames,envir=cache)
    cachePrefix <- strsplit(cacheName,"PathCache",fixed=T)[[1]][1]
    #resourceId
    resourceIdCacheName <- paste0(cachePrefix,"IdCache")
    if (resourceIdCacheName %in% caches) {
      resourceIdCache <- cacheEnv[[resourceIdCacheName]]
      rm(list=idDf$resourceId,envir=resourceIdCache)
    }
    if (deleteLinkedFiles) {
      if (!is.null(cacheEnv$createdLinks)) {
          cleaned <- NULL
          for (j in 1:nrow(cacheEnv$createdLinks)) {
            linkEntry <- cacheEnv$createdLinks[j,]
            if (linkEntry$resourceId %in% idDf$resourceId) {
              log_warn("deleting local link to deleted file")
              unlink(linkEntry$localPath)
            }
            else {
              cleaned <- plyr::rbind.fill(cleaned,linkEntry)
            }
          }
          cacheEnv$createdLinks<-cleaned
          saveImproveJson()

      }
    }
    entityIdCacheName <- paste0(cachePrefix,"EntityIdCache")
    if (entityIdCacheName %in% caches) {
      entityIdCache <- cacheEnv[[entityIdCacheName]]
      rm(list=idDf$entityId,envir=entityIdCache)
    }
    entityVersionIdCacheName <- paste0(cachePrefix,"EntityVersionIdCache")
    if (entityVersionIdCacheName %in% caches) {
      entityVersionIdCache <- cacheEnv[[entityVersionIdCacheName]]
      rm(list=idDf$entityVersionId,envir=entityVersionIdCache)
    }
  }
}
