Sys.setenv(TEST_NAME="steps")

# Helper function to ensure TEST_FOLDER exists when running tests individually
ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME="steps")
  if (!exists("TEST_FOLDER") || is.null(TEST_FOLDER)) {
    improveR::setEditable(TRUE)
    TEST_FOLDER <- improveR:::workflowFilesSetup()
    assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
    return(TEST_FOLDER)
  }
  return(TEST_FOLDER)
}

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

mockStep <- function(tree, dataSet, name, description, dataSet2 = NULL, dataSet3 = NULL) {
  stepEnv <- rBatchStep(tree)
  stepEnv$setStepDescription(name)
  stepEnv$setStepRationale(description)
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"), variableName = "command-file")
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.Rmd"))
  stepEnv$addStepRemoteFile(dataSet, name = "data.csv")

  if (!is.null(dataSet2)) {
    stepEnv$addStepRemoteFile(dataSet2, name = "data2.csv")
  }
  if (!is.null(dataSet3)) {
    stepEnv$addStepRemoteFile( dataSet3, name = "data3.csv")
  }
  realStep <- stepEnv$realise(run=F)

  # inventory <- realStep$getStepInventory()$data[[1]]
  # dataSet <- inventory %>%
  #   dplyr::filter(name == "chapter15_example_cleaned.rds") %>%
  #   dplyr::select("entityId") %>% as.character()
  # return(dataSet)
}


test_that("test for attaching step to itself", {
  TEST_FOLDER <- ensureTestFolder()

  testTree <- improveR::createAnalysisTree(
    targetIdent = TEST_FOLDER,
    treeName = "stepsTree"
  )

  stepCreated <- createStep(treeIdent = testTree)

  #attach step to itself
  stepTestAttached <- attachStep(
    ident = stepCreated$resourceId,
    parent = stepCreated$resourceId
  )

  #should not have a child resource which has the same ident as itself
  stepChildrenIds <- loadChildSteps(
    ident = stepTestAttached$resourceId
  )$data[[1]]$entityId
  expect_false(stepCreated$entityId %in% stepChildrenIds)

  #resource cannot be its own parent
  expect_false(stepTestAttached$resourceId == stepTestAttached$parentId)

  #step remains unchanged after trying to attach it to itself
  expect_true(identical(stepCreated, stepTestAttached))
})