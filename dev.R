
devtools::document()
devtools::load_all()
improveR::clearConnectionData()
improVerticleDevelopment:::login()
delete("/Projects/Tests/runFiles")
TEST_FOLDER <- workflowFilesSetup()
library(magrittr)

rBatchStep <- function(testTree) {
  r_runserver <- Sys.getenv("R_RUNSERVER")
  r_tool <- Sys.getenv("R_TOOL")
  r_tool_instance <- Sys.getenv("R_TOOL_INSTANCE")

  stepEnv <- createStepTemplateEnv(treeIdent=testTree)
  stepEnv$setStepRunserverLabel(r_runserver)
  stepEnv$setStepToolLabel(r_tool)
  stepEnv$setStepToolInstance(r_tool_instance)

  return(stepEnv)
}

nonmemBatchStep <- function(testTree) {
  nonmem_runserver <- Sys.getenv("NONMEM_RUNSERVER")
  nonmem_tool <- Sys.getenv("NONMEM_TOOL")
  nonmem_tool_instance <- Sys.getenv("NONMEM_TOOL_INSTANCE")


  stepEnv <- createStepTemplateEnv(treeIdent=testTree)
  stepEnv$setStepRunserverLabel(nonmem_runserver)
  stepEnv$setStepToolLabel(nonmem_tool)
  stepEnv$setStepToolInstance(nonmem_tool_instance)



  #setStepCommandLine("<command-file>\r\noutput<process>.txt", append = F) %>%

  return(stepEnv)
}
#reporting
#stepEnv <- getStep("repo_alias_todo:ST-713")
#data import
#stepEnv <- getStep("repo_alias_todo:ST-655")

#workflowEnv <- getWorkflow("/Projects/demoDev/template1/LinearModeling")



stepEnv <- getStep("repo_alias_todo:ST-713")


reportingTemplate <- stepEnv$createTemplate()
reportingTemplate$realise()




