#' helper function to validate the resource's existence
#' @param resourceId id (UUID) of the resource whose existence is to be checked
#' @noRd
validateResource <- function(resourceId) {
  resource <- tryCatch({
    loadResource(resourceId)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(resource)) {
    log_error("The resource with the id:", resourceId, "does not exist")
    return(FALSE)
  }
  return(TRUE)
}

#' Creates a New Review
#' @param name name of the review
#' @param parentPath path of the parent
#' @param comment review comment
#' @param templateId id (UUID) of the template
#' @param resourceIds vector of resourceIds
#' @param reviewerIds vector of reviewerIds
#' @param dueDate format: yyyy-mm-dd
#' @references ics1527
#' @export
createReview <- function(name, parentPath, comment, templateId, resourceIds, reviewerIds, dueDate) {
  improveEditable()

  if (any(!sapply(resourceIds, validateResource))) {
    return(NULL)
  }

  data <- list("name" = name,
               "parentPath" = parentPath,
               "comment" = comment,
               "templateId" = templateId,
               "resourceIds" = resourceIds,
               "reviewerIds" = reviewerIds,
               "dueDate" = dueDate)

  result <- authenticatedREST("/reviews", data = data, restType = "POST")
  updateReviews()

  return(result)
}
