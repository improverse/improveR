#' helper function to validate the resource's existence
#' @param resourceId id (UUID) of the resource whose existence is to be checked
#' @noRd
validateResource <- function(resourceId) {
  resource <- loadResource(resourceId)

  if (is.null(resource)) {
    log_error("The resource with the id:", resourceId, "does not exist")
    return(FALSE)
  }
  return(TRUE)
}

#' Creates a New Review
#'
#' Creates a review resource under the specified parent folder, containing
#' the given resources and assigned to the given reviewers.
#'
#' @param name Character. Name of the review.
#' @param parentIdent Identifier of the parent folder. Can be a path, resource ID,
#'   entity ID, or a data frame row from \code{loadResource()}.
#' @param comment Character. Review comment.
#' @param templateId Character. ID (UUID) of the review template.
#' @param resourceIds Character vector of resource IDs (UUIDs) to include in the review.
#' @param reviewerIds Character vector of reviewer user IDs (UUIDs).
#' @param dueDate Character. Due date in \code{yyyy-mm-dd} format.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame with the created review resource, or \code{NULL} on failure.
#' @references ics1527
#' @export
createReview <- function(name, parentIdent, comment, templateId, resourceIds, reviewerIds, dueDate, from = pwd()) {
  improveEditable()

  parentResource <- loadResource(parentIdent, from)
  if (is.null(parentResource)) {
    log_warn("cannot find parent resource by ident:", parentIdent)
    return(NULL)
  }

  if (any(!sapply(resourceIds, validateResource))) {
    log_warn("one or more resources to include in the review do not exist")
    return(NULL)
  }

  data <- list("name" = name,
               "parentPath" = parentResource$path,
               "comment" = comment,
               "templateId" = templateId,
               "resourceIds" = resourceIds,
               "reviewerIds" = reviewerIds,
               "dueDate" = dueDate)

  result <- authenticatedREST("/reviews", data = data, restType = "POST")
  updateReviews()

  if (is.null(result)) {
    log_warn("failed to create review '", name, "' - server returned no result")
    return(NULL)
  }
  cont <- httr::content(result)
  resourceId <- if (!is.null(cont$resourceId)) cont$resourceId else cont$id
  if (is.null(resourceId)) {
    log_warn("failed to extract resource ID from created review response")
    return(NULL)
  }
  return(loadResource(resourceId))
}
