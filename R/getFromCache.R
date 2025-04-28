removeFromCache <- function(key,argument,cacheList) {
  if (!is.null(key)) {
    logging::logdebug(paste0("Removing ",key," from Caches"))
    if (cacheEnv$persistentCaching & cacheEnv$reproducible) {
      cacheEnv$reproducible<-F
      logging::logwarn("No longer reproducible, as cache was updated during working")
    }
    res <- getFromCache(key,function(key){},cacheList,"")
    devnull<-lapply(names(cacheList),function(cacheName) {
      if (cacheName %in% ls(envir=cacheEnv)) {
        cache<-get(cacheName,envir=cacheEnv)
        cacheKey <- unname(cacheList[cacheName])[[1]]
        if (is.character(cacheKey)) {
          value <- as.character(unique(res[cacheKey]))
          #if (cacheKey=="entityId" | cacheKey=="entityVersionId"){
          #  value <- strsplit(value,":")[[1]][2]
          #}
          rm(list = value ,envir=cache)
        }
        else if (is.function(cacheKey)) {
          value <- cacheKey(res,argument)
          rm(list = ls(pattern = value,envir=cache),envir=cache)
        }
      }
    })
  }
}


getFromCache <- function(key,func,cacheList,argument,...) {
  improveConnected()
  if (is.character(key)) {
    logging::logdebug(paste0("Retrieving ",key," from caches"))
  } else {
    logging::logdebug(paste0("Retrieving from caches by function"))
  }
  initialiseCache(cacheList)
  val <- searchInCache(cacheList,key)
  if (!is.null(val)) {
    logging::logdebug("Found")
    return(val)
  } else {
    logging::logdebug("Get for cache ")
    val <- func(key,...)
    if (!is.null(val)) {
      writeToCache(val,cacheList,argument)
    }
  }
  return(val)
}

searchInCache <- function(resourceCacheList,searchString) {
  for (i in 1:length(names(resourceCacheList))) {
    sString <-searchString
    cacheName <- names(resourceCacheList)[i]
    cache <-get(cacheName,envir=cacheEnv)
    cacheKey <- unname(resourceCacheList[cacheName])[[1]]
    if (is.character(cacheKey)) {
      #if (cacheKey=="entityId" | cacheKey=="entityVersionId"){
      #  sString <- strsplit(sString,":")[[1]][2]
      #}
      returnVal <- get0(sString,envir=cache)
      if (!is.null(returnVal)) {
        return(returnVal)
      }
    }
    else if (is.function(cacheKey)) {
      searchStringTransformed<-cacheKey(pwd(),searchString)
      returnVal <- get0(searchStringTransformed,envir=cache)
      if (!is.null(returnVal)) {
        return(returnVal)
      }
    }
  }
  return(NULL)
}

writeToCache <- function(res,resourceCacheList,argument) {
  devnull<-lapply(names(resourceCacheList),function(cacheName) {
    if (cacheName %in% ls(envir=cacheEnv)) {
      cache<-get(cacheName,envir=cacheEnv)
      cacheKey <- unname(resourceCacheList[cacheName])[[1]]
      if (is.character(cacheKey)) {
        value <- as.character(unique(res[cacheKey]))
        #if (cacheKey=="entityId" | cacheKey=="entityVersionId"){
        #  value <- strsplit(value,":")[[1]][2]
        #  print(value)
        #}
        assign(value,res,envir=cache)
      }
      else if (is.function(cacheKey)) {
        value <- cacheKey(res,argument)
        assign(value,res,envir=cache)
      }
    }
  })
}


initialiseCache <- function(cacheList) {
  lapply(names(cacheList),function(cacheName) {
    if (!cacheName %in% ls(envir=cacheEnv)) {
      cache<-new.env(parent=emptyenv())
      assign(cacheName,cache,envir=cacheEnv)
    }
  })
}

#' resetCache
#' @description Empties the entire cache and removes a cacheFile, if it exists.
#' Sets the step to non-reproducible. The reset cache is similar to
#' its state after calling improveConnect.
#' @references ics1091
#' @seealso [improveConnect()]
#' @export
resetCache <- function() {
  logLevel<-cacheEnv$logLevel
  secure<-cacheEnv$secure
  offlinePossible<-cacheEnv$offlinePossible
  persistentCaching<-cacheEnv$persistentCaching
  if (file.exists(".improver.cache")) {
    file.remove(".improver.cache")
  }
  authenticationProvider <- cacheEnv$authenticationProvider
  rm(list=ls(envir=cacheEnv),envir=cacheEnv)
  cacheEnv$authenticationProvider <- authenticationProvider
  improveConnect(logLevel = logLevel,secure = secure,offlinePossible = offlinePossible,persistentCaching = persistentCaching)
  cacheEnv$reproducible<-F
}
