
#resetCache()
#fullDemoWorkflow <- getFullDependencies("<repository-prefix>:ST-69749") %>%
#  makeStepsRelative() %>%
#  detachWorkflowFromResources() %>%
#  detachWorkflowFromTrees() %>%
#  setWorkflowTreeRootFolder("/demos/MWintegration3") %>%
#  retrieveWorkflow()


#fullDemoWorkflow$breakpoint<-T

#jsonlite::write_json(fullDemoWorkflow,"demoWorkflow.json",pretty=T)

#executeWorkflow(fullDemoWorkflow)
