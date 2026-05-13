#' Compute execution order for workflow steps based on dependencies
#'
#' Topological sort of workflow steps: steps with no dependencies come first,
#' then steps whose dependencies are all already placed, recursively.
#' Detects cycles by checking whether progress is made each iteration
#' rather than using a fixed counter, so arbitrarily large workflows are supported.
#'
#' @param plan A data.frame with at least columns `fullName`, `dependencies`
#'   (comma-separated step names or NA), and `usage` (comma-separated step
#'   names or NA).
#' @param startSteps Optional data.frame of already-ordered steps (used in
#'   recursive calls).
#' @param .prevPlanSize Internal parameter to detect stalls. Do not set manually.
#' @return A data.frame of steps in execution order, or NULL if no valid
#'   ordering exists.
#' @keywords internal
workflowExecutionOrder <- function(plan, startSteps = NULL, .prevPlanSize = -1) {
  if (!"dependencies" %in% names(plan)) {
    plan$dependencies <- NA
  }
  if (!"usage" %in% names(plan)) {
    plan$usage <- NA
  }
  if (is.null(startSteps)) {
    startSteps <- plan[is.na(plan$dependencies), ]
    plan <- plan[!is.na(plan$dependencies), ]
  }
  if (is.null(startSteps) || nrow(startSteps) == 0) {
    log_warn("No step without dependencies, no executable order")
    return(NULL)
  }

  for (s in seq_len(nrow(startSteps))) {
    startStep <- startSteps[s, ]
    if (!is.na(startStep$usage)) {
      usageSteps <- strsplit(startStep$usage, ",", fixed = TRUE)[[1]]
      if (length(usageSteps) > 0) {
        for (dependentStepName in usageSteps) {
          candidateStep <- plan[plan$fullName == dependentStepName, ]
          if (nrow(candidateStep) == 1 && "dependencies" %in% names(candidateStep)) {
            stepDependencies <- strsplit(
              candidateStep$dependencies, ",", fixed = TRUE
            )[[1]]
            if (all(stepDependencies %in% startSteps$fullName)) {
              startSteps <- plyr::rbind.fill(startSteps, candidateStep)
              plan <- plan[plan$fullName != dependentStepName, ]
            }
          }
        }
      }
    }
  }

  if (nrow(plan) == 0) {
    return(startSteps)
  }

  # Detect stall: if plan didn't shrink since last iteration, we have a cycle
  if (nrow(plan) == .prevPlanSize) {
    cycleSteps <- paste(plan$fullName, collapse = ", ")
    stop(
      "Circular dependency detected in workflow. ",
      "The following steps have unresolvable dependencies: ",
      cycleSteps
    )
  }

  workflowExecutionOrder(plan, startSteps, .prevPlanSize = nrow(plan))
}
