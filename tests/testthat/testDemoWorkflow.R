
resetCache()
fullDemoWorkflow <- getFullLineage("envhost1.hc.scintecodev.internal-5310:ST-69749") %>%
  makeStepsRelative() %>%
  detachWorkflowFromResources() %>%
  detachWorkflowFromTrees() %>%
  setWorkflowTreeRootFolder("/demos/MWintegration3") %>%
  retrieveWorkflow()


fullDemoWorkflow$breakpoint<-T

jsonlite::write_json(fullDemoWorkflow,"demoWorkflow.json",pretty=T)

executeWorkflow(fullDemoWorkflow)
