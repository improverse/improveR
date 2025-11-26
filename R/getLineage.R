#' Get dependencies (dependencies) for a step
#'
#' Version-aware function that retrieves step dependencies.
#' For version 4.4+, uses the /dependencies REST endpoint.
#' For version < 4.4, uses the deprecated implementation.
#'
#' @param step The step resource object
#' @param env The step environment
#' @return List of dependencies steps
#' @keywords internal
#' @noRd
getDependencies <- function(step, env) {
  # Check repository version
  repoVersion <- getRepositoryVersion()

  if (!is.null(repoVersion)) {
    # Parse major.minor from version string
    versionParts <- strsplit(repoVersion, "[.-]")[[1]]
    if (length(versionParts) >= 2) {
      majorMinor <- as.numeric(paste0(versionParts[1], ".", versionParts[2]))

      if (majorMinor >= 4.4) {
        # Use new REST endpoint
        dependenciesResult <- authenticatedREST(
          "/resources/{resourceId}/dependencies",
          urlParams = list(resourceId = step$resourceId)
        )
        if (
          !is.null(dependenciesResult) && dependenciesResult$status_code == 200
        ) {
          return(httr::content(dependenciesResult))
        }
        return(list())
      }
    }
  }

  # Default to deprecated version for older repositories
  return(getDependencies_deprecated(step, env))
}

#' Get dependencies for version < 4.4
#'
#' Gets all links in the step, finds their targets, then traverses up
#' the resource hierarchy to find which steps contain those targets.
#'
#' @keywords internal
#' @noRd
getDependencies_deprecated <- function(step, env) {
  dependenciesSteps <- list()
  foundStepIds <- character()

  # Get all links in the current step
  inventory <- getStepResourceInventory(step, recurse = TRUE)
  if (is.null(inventory) || is.null(inventory$data[[1]])) {
    return(dependenciesSteps)
  }

  inventoryDf <- inventory$data[[1]]

  # Filter for links
  links <- inventoryDf[
    inventoryDf$nodeType == "Link" | inventoryDf$nodeType == "LIV",
  ]

  if (nrow(links) > 0) {
    # For each link, find which step contains the target
    for (i in 1:nrow(links)) {
      link <- links[i, ]

      # Get the target of the link
      targetId <- if (!is.null(link$targetEntityId)) {
        link$targetEntityId
      } else {
        link$targetId
      }
      if (is.null(targetId) || is.na(targetId)) {
        next
      }

      # Use the existing findContainerStep function
      containingStep <- findContainerStep(targetId)

      if (
        !is.null(containingStep) &&
          containingStep$entityId != step$entityId &&
          !(containingStep$entityId %in% foundStepIds)
      ) {
        dependenciesSteps <- append(dependenciesSteps, list(containingStep))
        foundStepIds <- c(foundStepIds, containingStep$entityId)
      }
    }
  }

  return(dependenciesSteps)
}