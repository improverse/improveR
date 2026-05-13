#' Load Favorites
#'
#' Retrieves the current user's favorites collection from the repository.
#'
#' @returns A data frame of favorite resources, or \code{NULL} if none exist.
#' @references ics1799
#' @export
loadFavorites <- function() {
  improveConnected()
  return(restGetAsDf("/favorites"))
}

#' Load Favorite Children
#'
#' Retrieves the child resources of a favorites folder. If \code{parentId} is
#' \code{NULL}, returns the top-level favorite resources.
#'
#' @param parentId Optional resource ID of a favorites folder. If \code{NULL},
#'   retrieves top-level favorites.
#' @returns A data frame of favorite child resources, or \code{NULL} if none exist.
#' @references ics1801, ics1802
#' @export
loadFavoriteChildren <- function(parentId = NULL) {
  improveConnected()
  if (is.null(parentId)) {
    url <- "/favorites/resources"
  } else {
    url <- "/favorites/{parentId}/resources"
  }
  return(restGetAsDf(url, urlParams = list(parentId = parentId)))
}

#' Add Favorite Link
#'
#' Creates a link to an existing resource inside the user's favorites collection.
#'
#' @param targetIdent Identifier of the resource to add as a favorite. Can be a
#'   path, resource ID, entity ID, or a data frame row from \code{loadResource()}.
#' @param name Display name for the favorite link.
#' @param parentId Optional resource ID of the parent favorites folder. If
#'   \code{NULL}, the link is created at the top level.
#' @param comment Optional comment for the operation.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame with the created favorite link details, or \code{NULL} on failure.
#' @references ics1803, ics1804
#' @export
addFavoriteLink <- function(targetIdent, name, parentId = NULL, comment = "", from = pwd()) {
  improveEditable()
  targetResource <- loadResource(targetIdent, from)
  if (is.null(targetResource)) {
    log_warn("cannot find target resource by ident:", targetIdent)
    return(NULL)
  }
  if (is.null(parentId)) {
    url <- "/favorites/links"
  } else {
    url <- "/favorites/{parentId}/links"
  }
  linkData <- list(targetId = targetResource$resourceId, name = name, comment = comment)
  result <- authenticatedREST(url,
                              urlParams = list(parentId = parentId),
                              data = list(linkData),
                              restType = "POST")
  if (is.null(result)) {
    log_warn("Failed to add favorite link for:", targetIdent)
    return(NULL)
  }
  cont <- httr::content(result)
  if (is.list(cont) && length(cont) > 0 && is.null(names(cont))) {
    cont <- cont[[1]]
  }
  df <- as.data.frame(cont, stringsAsFactors = FALSE)
  return(df)
}

#' Create Favorite Folder
#'
#' Creates a new folder inside the user's favorites collection for organising
#' favorite resources.
#'
#' @param name Name of the new favorites folder.
#' @param parentId Optional resource ID of the parent favorites folder. If
#'   \code{NULL}, the folder is created at the top level.
#' @param comment Optional comment for the operation.
#' @returns A data frame with the created favorites folder details, or \code{NULL} on failure.
#' @references ics1805, ics1806
#' @export
createFavoriteFolder <- function(name, parentId = NULL, comment = "") {
  improveEditable()
  if (is.null(parentId)) {
    url <- "/favorites/folder"
  } else {
    url <- "/favorites/{parentId}/folder"
  }
  data <- list(name = name, comment = comment)
  result <- authenticatedREST(url,
                              urlParams = list(parentId = parentId),
                              data = data,
                              restType = "POST")
  if (is.null(result)) {
    log_warn("Failed to create favorite folder:", name)
    return(NULL)
  }
  cont <- httr::content(result)
  if (is.null(cont)) {
    log_warn("Empty response when creating favorite folder:", name)
    return(NULL)
  }
  df <- as.data.frame(cont, stringsAsFactors = FALSE)
  return(df)
}

#' Remove Favorite
#'
#' Removes a resource from the user's favorites collection.
#'
#' @param resourceId Resource ID of the favorite to remove.
#' @returns \code{TRUE} if the favorite was removed successfully, \code{FALSE} otherwise.
#' @references ics1800
#' @export
removeFavorite <- function(resourceId) {
  improveEditable()
  result <- authenticatedREST("/favorites/{resourceId}",
                              urlParams = list(resourceId = resourceId),
                              restType = "DELETE")
  if (!is.null(result)) {
    return(TRUE)
  }
  log_warn("Failed to remove favorite:", resourceId)
  return(FALSE)
}
