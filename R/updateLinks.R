#' Update link resource(s)
#'
#' Updates one or more link resources by resourceId.
#' Only updates resources of type "Link".
#' After updating, unloads the parent resource and its children to clear cache.
#' @param links Vector of resource IDs or a data.frame with a column 'entityId'
#' @param comment Optional comment for the update
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
    linkRes <- improveR::loadResource(linkId)
    if (!identical(linkRes$nodeType, "Link")) next
    data <- list(nodeType = "Link", name = linkRes$name, comment = comment)
    improveR::authenticatedREST(
      "/resources/{resourceId}",
      queryParams = list(updateLink = "true"),
      urlParams = list(resourceId = linkRes$resourceId),
      restType = "PUT",
      data = data
    )
    # Unload parent and its children to clear cache
    if (!is.null(linkRes$parentId)) {
      improveR::unloadResource(linkRes$resourceId)
      improveR::unloadChildResources(linkRes$parentId)
    }
  }
  invisible(NULL)
}