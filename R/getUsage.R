#' Get usage (which steps use this step's outputs) for a step
#'
#' Version-aware function that retrieves steps that use this step's outputs.
#' For version 4.4+, uses the /usages REST endpoint.
#' For version < 4.4, uses the deprecated implementation.
#'
#' @param step The step resource object
#' @param env The step environment
#' @return List of steps that use this step's outputs
#' @keywords internal
#' @noRd
getUsage <- function(step, env) {
  step <- loadResource(step)
  # Check repository version
  repoVersion <- getRepositoryVersion()

  if (!is.null(repoVersion)) {
    # Parse major.minor from version string
    versionParts <- strsplit(repoVersion, "[.-]")[[1]]
    if (length(versionParts) >= 2) {
      majorMinor <- as.numeric(paste0(versionParts[1], ".", versionParts[2]))

      if (majorMinor >= 4.4) {
        # Use new REST endpoint
        usageResult <- authenticatedREST("/resources/{resourceId}/usages",
                                        urlParams = list(resourceId = step$resourceId))
        if (!is.null(usageResult)) {
          return(httr::content(usageResult))
        }
        return(list())
      }
    }
  }

  # Default to deprecated version for older repositories
  return(getUsage_deprecated(step, env))
}

#' Get usage for version < 4.4
#'
#' Gets all files in the step, loads their references, then traverses up
#' the resource hierarchy to find which steps contain those references.
#'
#' @keywords internal
#' @noRd
getUsage_deprecated <- function(step, env) {
  usageSteps <- list()
  foundStepIds <- character()

  # Get all files in the current step
  inventory <- getStepResourceInventory(step, recurse = TRUE)
  if (is.null(inventory) || is.null(inventory$data[[1]])) {
    return(usageSteps)
  }

  inventoryDf <- inventory$data[[1]]

  # Filter for files
  files <- inventoryDf[inventoryDf$nodeType == "File" | inventoryDf$nodeType == "FIV", ]

  if (nrow(files) > 0) {
    # For each file, get its references and find which steps contain them
    for (i in 1:nrow(files)) {
      file <- files[i, ]

      # Get references to this file
      references <- loadReferences(file$resourceId)$data[[1]]

      if (!is.null(references) && nrow(references) > 0) {
        # For each reference, find which step contains it
        for (j in 1:nrow(references)) {
          ref <- references[j, ]

          # Use the existing findContainerStep function
          containingStep <- findContainerStep(ref$resourceId)

          if (!is.null(containingStep) &&
              containingStep$entityId != step$entityId &&
              !(containingStep$entityId %in% foundStepIds)) {
            usageSteps <- append(usageSteps, list(containingStep))
            foundStepIds <- c(foundStepIds, containingStep$entityId)
          }
        }
      }
    }
  }

  return(usageSteps)
}
