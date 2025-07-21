
devtools::document()
devtools::load_all()
improveR::clearConnectionData()
improVerticleDevelopment:::login()
delete("/Projects/Tests/runFiles")

#reporting
#stepEnv <- getStep("repo_alias_todo:ST-713")
#data import
#stepEnv <- getStep("repo_alias_todo:ST-655")

#workflowEnv <- getWorkflow("/Projects/demoDev/template1/LinearModeling")



stepEnv <- getStep("repo_alias_todo:ST-713")


reportingTemplate <- stepEnv$createTemplate()
reportingTemplate$realise()




