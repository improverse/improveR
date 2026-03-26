allowedTargets <- list(
  "Folder"=c("Step","Folder","Analysis Tree"),
  "File"=c("Step","Folder","Analysis Tree"),
  "Analysis Tree"=c("Folder"),
  "ExtLink"=c("Step","Folder"),
  "Link"=c("Step","Folder","Analysis Tree"),
  "Step"=c("Analysis Tree")
)

isAllowedTarget <- function(targetType,nodeType,logWarning=F) {
  allowed <- targetType %in% allowedTargets[[nodeType]]
  if (logWarning && !allowed) {
    log_warn(targetType, "is not an allowed target type for",nodeType)
  }
  return(allowed)
}
