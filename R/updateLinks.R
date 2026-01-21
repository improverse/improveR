#' Update Link Resources to Latest Target Version
#'
#' Refreshes one or more link resources to point to the latest version of their targets.
#' Links in improve always reference the most recent version of a resource, but this function
#' explicitly triggers an update and cache refresh, useful when target resources have changed
#' and you want to ensure links are synchronized.
#'
#' @param links Identifier(s) of link resource(s) to update. Can be:
#'   \itemize{
#'     \item Vector of resource IDs or entity IDs
#'     \item Data frame with an \code{entityId} column (extracts IDs automatically)
#'   }
#' @param comment Character. Optional comment describing the update reason.
#'   Defaults to "update outdated".
#'
#' @return Invisible \code{NULL}. Called for its side effect of updating link resources
#'   and clearing related caches.
#'
#' @details
#' The function processes each link by:
#' \enumerate{
#'   \item Loading the resource and verifying it's a Link node type
#'   \item Sending an update request to the server with \code{updateLink=true}
#'   \item Clearing caches for the link and its parent folder
#' }
#'
#' Only resources of type "Link" are updated - other node types are silently skipped.
#' This allows you to safely pass a mixed set of resources and only links will be refreshed.
#'
#' The function handles duplicate IDs automatically, processing each unique link only once.
#'
#' After updating, the parent folder's child cache is cleared to ensure subsequent
#' queries return fresh data reflecting the updated links.
#'
#' @seealso
#' \code{\link{createLink}} to create new links,
#' \code{\link{loadReferences}} to view link relationships,
#' \code{\link{delete}} to remove outdated links
#'
#' @examples
#' \dontrun{
#' # Update a single link
#' updateLinks("/Workflows/Step1/data_link")
#'
#' # Update multiple links
#' updateLinks(c(
#'   "/Workflows/Step1/data_link",
#'   "/Workflows/Step2/data_link"
#' ))
#'
#' # Update all links in a workflow found by query
#' links <- query("nodeType='Link' AND path='/Workflows/Analysis/*'")
#' updateLinks(links, comment = "refresh after data update")
#'
#' # Update using entity IDs from a data frame
#' link_df <- data.frame(
#'   entityId = c("abc123", "def456"),
#'   name = c("link1", "link2")
#' )
#' updateLinks(link_df)
#' }
#'
#' @export
updateLinks <- function(links, comment = "update outdated") {
  if (is.null(links) || length(links) == 0) return(invisible(NULL))
  # Accept data.frame with entityId column
  if (is.data.frame(links) && "entityId" %in% names(links)) {
    links <- links$entityId
  }
  # Accept list of IDs
  links <- unique(unlist(links))
  for (linkId in links) {
    linkRes <- loadResource(linkId)
    if (!identical(linkRes$nodeType, "Link")) next
    data <- list(nodeType = "Link", name = linkRes$name, comment = comment)
    authenticatedREST(
      "/resources/{resourceId}",
      queryParams = list(updateLink = "true"),
      urlParams = list(resourceId = linkRes$resourceId),
      restType = "PUT",
      data = data
    )
    # Unload parent and its children to clear cache
    if (!is.null(linkRes$parentId)) {
      unloadResource(linkRes$resourceId)
      unloadChildResources(linkRes$parentId)
    }
  }
  invisible(NULL)
}
