#' Clone Child Step CLI
#'
#' Clones a child step from the CLI.
#'
#' @param localSource The local source repository.
#' @param localTarget The local target repository.
#' @param comment A comment for the clone operation.
#' @param toolCategory The tool category. Default is NULL.
#' @param tool The tool name. Default is NULL.
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

  command <- glue::glue("clone childstep -sourceRepository {localSource} -targetRepository {localTarget} -tool {tool} -toolCategory {toolCategory} -comment {comment}")
  executeCli(command)
}
