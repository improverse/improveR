#' Get Latest Run
#'
#' Retrieves the latest run for a process belonging to a step.
#'
#' @param ident Step identifier (path, resource ID, entity ID, etc.).
#' @param processId The ID of the process.
#' @returns A data frame with the latest run data, or \code{NULL} if not found.
#' @references ics1329
#' @export
getLatestRun <- function(ident, processId) {
  improveConnected()
  res <- loadResource(ident)
  if (is.null(res)) {
    log_warn("Cannot find step by ident:", ident)
    return(NULL)
  }
  result <- authenticatedREST(
    "/resources/{resourceId}/processes/{processId}/runs/latest",
    urlParams = list(resourceId = res$resourceId,
                     processId = processId),
    restType = "GET")
  if (is.null(result)) {
    log_warn("Failed to get latest run for step:", ident, "process:", processId)
    return(NULL)
  }
  cont <- httr::content(result)
  if (length(cont) == 0) {
    log_warn("No runs found for step:", ident, "process:", processId)
    return(NULL)
  }
  return(as.data.frame(cont, stringsAsFactors = FALSE))
}

#' Get Run
#'
#' Retrieves a specific run by its ID for a process belonging to a step.
#'
#' @param ident Step identifier (path, resource ID, entity ID, etc.).
#' @param processId The ID of the process.
#' @param runId The ID of the run.
#' @returns A data frame with the run data, or \code{NULL} if not found.
#' @references ics1330
#' @export
getRun <- function(ident, processId, runId) {
  improveConnected()
  res <- loadResource(ident)
  if (is.null(res)) {
    log_warn("Cannot find step by ident:", ident)
    return(NULL)
  }
  result <- authenticatedREST(
    "/resources/{resourceId}/processes/{processId}/runs/{runId}",
    urlParams = list(resourceId = res$resourceId,
                     processId = processId,
                     runId = runId),
    restType = "GET")
  if (is.null(result)) {
    log_warn("Failed to get run:", runId, "for step:", ident)
    return(NULL)
  }
  cont <- httr::content(result)
  if (length(cont) == 0) {
    log_warn("Run not found:", runId, "for step:", ident)
    return(NULL)
  }
  return(as.data.frame(cont, stringsAsFactors = FALSE))
}

#' Get Run Phases
#'
#' Retrieves the phases of a specific run for a process belonging to a step.
#'
#' @param ident Step identifier (path, resource ID, entity ID, etc.).
#' @param processId The ID of the process.
#' @param runId The ID of the run.
#' @returns A data frame with the run phases, or \code{NULL} if not found.
#' @references ics1333
#' @export
getRunPhases <- function(ident, processId, runId) {
  improveConnected()
  res <- loadResource(ident)
  if (is.null(res)) {
    log_warn("Cannot find step by ident:", ident)
    return(NULL)
  }
  result <- authenticatedREST(
    "/resources/{resourceId}/processes/{processId}/runs/{runId}/phases",
    urlParams = list(resourceId = res$resourceId,
                     processId = processId,
                     runId = runId),
    restType = "GET")
  if (is.null(result)) {
    log_warn("Failed to get run phases for run:", runId, "step:", ident)
    return(NULL)
  }
  cont <- httr::content(result)
  if (length(cont) == 0) {
    log_warn("No phases found for run:", runId, "step:", ident)
    return(NULL)
  }
  return(mergeListToDataframe(cont))
}

#' Get Run Parameters
#'
#' Retrieves the parameters of a specific run for a process belonging to a step.
#'
#' @param ident Step identifier (path, resource ID, entity ID, etc.).
#' @param processId The ID of the process.
#' @param runId The ID of the run.
#' @returns A data frame with the run parameters, or \code{NULL} if not found.
#' @references ics1334
#' @export
getRunParameters <- function(ident, processId, runId) {
  improveConnected()
  res <- loadResource(ident)
  if (is.null(res)) {
    log_warn("Cannot find step by ident:", ident)
    return(NULL)
  }
  result <- authenticatedREST(
    "/resources/{resourceId}/processes/{processId}/runs/{runId}/parameters",
    urlParams = list(resourceId = res$resourceId,
                     processId = processId,
                     runId = runId),
    restType = "GET")
  if (is.null(result)) {
    log_warn("Failed to get run parameters for run:", runId, "step:", ident)
    return(NULL)
  }
  cont <- httr::content(result)
  if (length(cont) == 0) {
    log_warn("No parameters found for run:", runId, "step:", ident)
    return(NULL)
  }
  return(mergeListToDataframe(cont))
}

#' Get Run Grid Arguments
#'
#' Retrieves the grid arguments of a specific run for a process belonging to a step.
#'
#' @param ident Step identifier (path, resource ID, entity ID, etc.).
#' @param processId The ID of the process.
#' @param runId The ID of the run.
#' @returns A data frame with the run grid arguments, or \code{NULL} if not found.
#' @references ics1332
#' @export
getRunGridArguments <- function(ident, processId, runId) {
  improveConnected()
  res <- loadResource(ident)
  if (is.null(res)) {
    log_warn("Cannot find step by ident:", ident)
    return(NULL)
  }
  result <- authenticatedREST(
    "/resources/{resourceId}/processes/{processId}/runs/{runId}/gridArguments",
    urlParams = list(resourceId = res$resourceId,
                     processId = processId,
                     runId = runId),
    restType = "GET")
  if (is.null(result)) {
    log_warn("Failed to get grid arguments for run:", runId, "step:", ident)
    return(NULL)
  }
  cont <- httr::content(result)
  if (length(cont) == 0) {
    log_warn("No grid arguments found for run:", runId, "step:", ident)
    return(NULL)
  }
  return(mergeListToDataframe(cont))
}
