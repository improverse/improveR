#' Clone Child Step CLI
#'
#' Creates a child step on the server and clones it to a fresh local repository.
#'
#' @param localSource The local source repository.
#' @param localTarget The local target repository.
#' @param comment A comment for the clone operation.
#' @param toolCategory The tool category. Default is NULL (uses parent step's category).
#' @param tool The tool name. Default is NULL (uses parent step's tool).
#' @noRd
cloneChildstepCli <- function(localSource, localTarget, comment, toolCategory = NULL, tool = NULL) {
  checkInit()
  step <- getLocalRepoResource(localSource)
  if (step$nodeType != "Step") {
    stop(paste(localSource, "is not of type Step"))
  }
  if (is.null(toolCategory)) {
    toolCategory <- step$toolCategory
  }
  if (is.null(tool)) {
    tools <- loadAllTools()
    tool <- tools[tools$id == step$toolId, ]$name
  }
  renewAccessToken()
  token <- conf()$reqToken

  args <- c("clone", "childstep",
            "-accessToken", token,
            "-sourceRepository", localSource,
            "-targetRepository", localTarget,
            "-tool", tool,
            "-toolCategory", toolCategory,
            "-comment", comment)
  executeCli(args)
}
