childResourceCacheList <- createCacheList("child")


#' Load Child Resources
#'
#' Loads all child resources for a given resource identifier. Child resources
#' can be folders, steps, workflows and analysis trees, but also the elements of a step's
#' inventory. Results are cached for performance.
#'
#' @param ident The resource identifier - can be a resource ID, entity ID,
#'   entity version ID, or path. Can also be a list of identifiers.
#' @param from Reference point for relative paths. Defaults to current
#'   working directory via \code{\link{pwd}}.
#'
#' @returns A list with class "improveResource" containing:
#'   \itemize{
#'     \item \code{type} - Type indicator ("child")
#'     \item \code{resourceId} - Unique resource identifier
#'     \item \code{entityId} - Entity identifier
#'     \item \code{entityVersionId} - Entity version identifier
#'     \item \code{path} - Full resource path
#'     \item \code{name} - Resource name
#'     \item \code{data} - A list containing data frames of child resources.
#'       Each data frame contains columns with resource metadata:
#'       \itemize{
#'         \item \code{resourceId}, \code{resourceVersionId} - Resource identifiers
#'         \item \code{nodeType} - Type of resource (e.g., "Step", "Analysis Tree", "File", "Folder")
#'         \item \code{name}, \code{path} - Resource name and full path
#'         \item \code{parentId} - Parent resource identifier
#'         \item \code{deleted} - Logical indicating deletion status
#'         \item \code{entityId}, \code{entityVersionId} - Entity identifiers
#'         \item \code{fullEntityId}, \code{fullEntityVersionId} - Full entity URLs
#'         \item \code{revisionId} - Revision identifier
#'         \item \code{createdByName}, \code{createdById} - Creator information
#'         \item \code{createdAt}, \code{createdAtDate} - Creation timestamp (milliseconds and POSIXct)
#'         \item \code{lastModifiedByName}, \code{lastModifiedById} - Last modifier information
#'         \item \code{lastModifiedOn}, \code{lastModifiedOnDate} - Last modification timestamp
#'         \item \code{fileSize} - File size in bytes
#'         \item \code{hasChildren}, \code{hasChildrenIncludingFiles} - Child resource indicators
#'         \item \code{outdatedLink} - Logical indicating outdated link status
#'         \item \code{finishedStatus} - Execution status (e.g., "unfinished")
#'         \item \code{workingFile} - Logical indicating working file status
#'       }
#'       Additional fields for specific node types:
#'       \itemize{
#'         \item For Steps: \code{keyStep}, \code{baseModel}, \code{fullModel},
#'           \code{finalModel}, \code{referenceModel}, \code{ownedById}, \code{ownedByName}
#'         \item For Files: \code{status}, \code{fileHash}
#'       }
#'   }
#'   Dates are converted to POSIXct format via \code{\link{convertImproveTimestampToPosix}}.
#'
#' @examples
#' \dontrun{
#' # Load children of current step
#' children <- loadChildResources(pwd())
#' children$data[[1]]
#'
#' # Load children by path
#' children <- loadChildResources("/Projects/MyWorkflow")
#' }
#'
#' @seealso
#' \code{\link{unloadChildResources}} to clear cache,
#' \code{\link{refreshChildResources}} to refresh from server,
#' \code{\link{loadChildSteps}} for step-specific children
#'
#' @export
loadChildResources <- function(ident,from=pwd()) {
  return(
    genericLoadResourceSubEntities(ident,
                                   func=loadChildResourcesFromServer,
                                   cacheList=childResourceCacheList,
                                   from=from)
  )
}

#' Unload Child Resources from Cache
#'
#' Removes child resources data from the cache for the specified resource.
#'
#' @param ident The resource identifier - can be a resource ID, entity ID,
#'   entity version ID, or path.
#'
#' @returns Invisibly returns NULL. Called for side effect of clearing cache.
#'
#' @seealso
#' \code{\link{loadChildResources}} to load child resources,
#' \code{\link{refreshChildResources}} to refresh from server
#'
#' @references ics1085
#' @export
unloadChildResources <- function(ident) {
  res <- loadResource(ident)
  removeFromCache(res$resourceId,"",childResourceCacheList)
}

#' Incrementally append a freshly created child resource to the parent's cached
#' children list. Used by createFolder/createFile/createLink to keep the cache
#' warm during bulk creation: each successful create previously called
#' unloadChildResources() which then forced the next createX in the loop to
#' re-fetch the children list from the server. For a step with N input files
#' that becomes O(N) round trips with N up to ~100s in real reports — the
#' existence-check overhead alone took ~60s for a PopPk render, blocking the
#' MCP transport timeout. With incremental update the cache stays consistent,
#' the next existence check sees the freshly-added child immediately, and the
#' bulk-create cost drops to a single initial load.
#'
#' If the parent has no cached entry, this is a no-op — the next loadChildResources
#' will fetch fresh from the server (correct behaviour, just no acceleration).
#'
#' @noRd
appendToChildResourcesCache <- function(parent, newChild) {
  if (is.null(parent) || is.null(newChild)) return(invisible(NULL))
  if (!is.data.frame(parent) || nrow(parent) != 1) return(invisible(NULL))
  if (!is.data.frame(newChild) || nrow(newChild) != 1) return(invisible(NULL))

  existing <- searchInCache(childResourceCacheList, parent$resourceId)
  if (is.null(existing)) {
    return(invisible(NULL))
  }

  childrenDf <- existing$data[[1]]
  if (is.null(childrenDf)) childrenDf <- data.frame()

  if (nrow(childrenDf) > 0 &&
      "resourceId" %in% names(childrenDf) &&
      !is.null(newChild$resourceId) &&
      newChild$resourceId %in% childrenDf$resourceId) {
    return(invisible(NULL))
  }

  childrenDf <- plyr::rbind.fill(childrenDf, newChild)
  existing$data <- list(childrenDf)
  writeToCache(existing, childResourceCacheList, NULL)
  invisible(NULL)
}

#' Walk a folder path under a parent resource, resolving each segment to the
#' existing child folder or creating it when missing. Used by realise() when
#' assembling a step's input/output file tree: a single step can have many
#' files landing in the same subfolder, and re-walking the path for every
#' file pays the loadChildResources + existence-check cost per file.
#'
#' Two layers of speedup vs the previous inlined walk:
#'   1. Existence check now reads $data[[1]]$name (the child names) instead of
#'      the resultFrame's $name column (the parent's name) — the previous
#'      inlined version always saw 0 matches and fell through to createFolder
#'      every time, which itself did the lookup, found the existing folder,
#'      logged "already exists" and returned. So 2 server-side checks per file
#'      per segment plus log spam.
#'   2. A session-level resolution cache keyed by (parent entityId, segment)
#'      keeps walked-and-resolved targets across calls, so the second file in
#'      the same subfolder pays nothing for the walk.
#'
#' @param parent  A 1-row resource data.frame (the start of the walk).
#' @param folderParts Character vector of folder names in path order.
#' @return The resolved (or newly created) leaf-folder resource, or NULL if
#'   any segment exists with a non-Folder nodeType.
#' @noRd
resolveFolderPath <- function(parent, folderParts) {
  if (is.null(cacheEnv$.folderResolutions)) {
    cacheEnv$.folderResolutions <- new.env(parent = emptyenv())
  }
  folderResCache <- cacheEnv$.folderResolutions

  currentTarget <- parent
  for (folderName in folderParts) {
    cacheKey <- paste0(currentTarget$entityId, ":", folderName)
    cached <- get0(cacheKey, envir = folderResCache)
    if (!is.null(cached)) {
      currentTarget <- cached
      next
    }

    childrenResult <- loadChildResources(currentTarget)
    childrenDf <- if (!is.null(childrenResult)) childrenResult$data[[1]] else NULL
    folder <- if (!is.null(childrenDf) && nrow(childrenDf) > 0) {
                childrenDf[childrenDf$name == folderName, , drop = FALSE]
              } else {
                data.frame()
              }

    if (nrow(folder) == 1 && folder$nodeType != "Folder") {
      log_warn(folderName, "already exists in", currentTarget$path, "but not as folder")
      return(NULL)
    }

    currentTarget <- if (nrow(folder) == 1) {
                       folder
                     } else {
                       createFolder(currentTarget, folderName = folderName)
                     }
    if (is.null(currentTarget)) return(NULL)

    assign(cacheKey, currentTarget, envir = folderResCache)
  }
  return(currentTarget)
}

#' Refresh Child Resources from Server
#'
#' Clears cached child resources data and reloads fresh data from the server.
#'
#' @param ident The resource identifier - can be a resource ID, entity ID,
#'   entity version ID, or path.
#'
#' @returns A list with updated child resources data. See \code{\link{loadChildResources}}
#'   for details on the return structure.
#'
#' @seealso
#' \code{\link{loadChildResources}} for return structure details,
#' \code{\link{unloadChildResources}} to only clear cache
#'
#' @param ... For backwards compatibility with the deprecated `update*` alias; not used by `refresh*` itself.
#' @references ics1085
#' @export
refreshChildResources <- function(ident) {
  unloadChildResources(ident)
  res <- loadChildResources(ident)
  return(res)
}

#' @rdname refreshChildResources
#' @export
updateChildResources <- function(...) {
  .Deprecated("refreshChildResources")
  refreshChildResources(...)
}

loadChildResourcesFromServer <- function(resource) {
  genericLoadFromServer(resource,name="child",funct=actualLoadChildResources)
}

actualLoadChildResources <- function(resource) {
  result<-NULL
  if (as.character(resource$resourceId)!="0") {
    result <- authenticatedREST("/resources/{resourceId}/resources", list(resourceId=resource$resourceId)
    )
  } else {
    result <- authenticatedREST("/resources")
  }
  if (is.null(result)) {
    return(data.frame())
  }
  cont <- httr::content(result)
  if (is.null(cont) || length(cont) == 0) {
    return(data.frame())
  }
  df <- mergeListToDataframe(cont)
  df <-convertDates(df)
  return(df)
}
