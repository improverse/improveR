Sys.setenv(TEST_NAME="workflowParameterization")

# Helper function to ensure TEST_FOLDER exists when running tests individually
ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME="workflowParameterization")
  if (!exists("TEST_FOLDER") || is.null(TEST_FOLDER)) {
    improveR::setEditable(TRUE)
    TEST_FOLDER <- improveR:::workflowFilesSetup()
    assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
    return(TEST_FOLDER)
  }
  return(TEST_FOLDER)
}

# Reuse mockStep helper from test-runSteps
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
  realStep <- stepEnv$realise()
  stepEnv$finishRun()

  inventory <- realStep$getStepInventory()$data[[1]]
  dataSet <- inventory %>%
    dplyr::filter(name == "chapter15_example_cleaned.rds") %>%
    dplyr::select("entityId") %>% as.character()
  return(dataSet)
}

library(magrittr)


test_that("parameterize workflow - change dataset in initial step|ics1213,imr166", {
  TEST_FOLDER <- ensureTestFolder()

  cat("\n=== BUILDING DMG WORKFLOW ===\n")

  # Create trees
  dmgL1 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "Param DMG L1")
  dmgL2 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "Param DMG L2")
  dmgL3 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "Param DMG L3")

  # Create workflow steps
  cat("Creating initial steps...\n")
  i1 <- mockStep(dmgL1, paste0(TEST_FOLDER, "/data.csv"), "Initial 1", "data comes to system")
  i2 <- mockStep(dmgL1, paste0(TEST_FOLDER, "/data.csv"), "Initial 2", "data comes to system")

  cat("Creating processing steps in L1...\n")
  s1t1 <- mockStep(dmgL1, i1, "S1T1", "processing", dataSet2 = i2)
  s2t1 <- mockStep(dmgL1, s1t1, "S2T1", "processing", dataSet2 = i2)

  cat("Creating processing steps in L2...\n")
  s1t2 <- mockStep(dmgL2, s1t1, "S1T2", "processing", dataSet2 = s2t1)
  s2t2 <- mockStep(dmgL2, s1t2, "S2T2", "processing", dataSet2 = i2)

  cat("Creating final step in L3...\n")
  s1t3 <- mockStep(dmgL3, s1t2, "S1T3", "report", dataSet2 = s2t2)

  cat("\n=== LOADING WORKFLOW WITH FULL LINEAGE ===\n")

  # Load the last step and its full lineage
  lastStep <- loadChildResources(dmgL3) %>% strip() %>% getStep()
  lastStep$lineage$load(stepDepth = -1, treeDepth = -1)

  # Create workflow template
  workflowTemplate <- lastStep$workflow$createTemplate()

  cat("\n=== EXAMINING WORKFLOW TEMPLATE ===\n")

  # Get all steps
  workflowDf <- workflowTemplate$df()
  cat("Workflow has", nrow(workflowDf), "steps\n")
  cat("Step descriptions:", paste(workflowDf$description, collapse=", "), "\n")

  # Find the "Initial 1" step
  initialSteps <- workflowDf[workflowDf$description == "Initial 1", ]
  expect_equal(nrow(initialSteps), 1, info = "Should find exactly one 'Initial 1' step")

  initialStepName <- initialSteps$fullName
  cat("Initial step name:", initialStepName, "\n")

  # Get the step template for Initial 1
  initialStepTemplate <- workflowTemplate$stepTemplates[[initialStepName]]
  expect_false(is.null(initialStepTemplate), info = "Should be able to access Initial 1 step template")

  # Check the remoteFiles in the initial step
  initialRemoteFiles <- initialStepTemplate$stepDf$remoteFiles[[1]]
  cat("\n=== INITIAL STEP REMOTE FILES ===\n")
  print(initialRemoteFiles[, c("name", "asLink", "ident")])

  # Find the data.csv file (name includes ./ prefix)
  dataFile <- initialRemoteFiles[initialRemoteFiles$name == "./data.csv", ]
  expect_equal(nrow(dataFile), 1, info = "Should find data.csv in initial step")

  originalIdent <- dataFile$ident
  cat("\nOriginal data.csv ident:", originalIdent, "\n")

  cat("\n=== CHANGING DATASET PARAMETER ===\n")

  # Now change the dataset using changeStepRemoteFile
  # First, we need a different file to change to - let's use DataManipulation.Rmd as a test
  testFileIdent <- loadResource(paste0(TEST_FOLDER, "/DataManipulation.Rmd"))$entityId
  cat("New test file ident:", testFileIdent, "\n")

  # Change the remote file (use full name with ./ prefix)
  initialStepTemplate$changeStepRemoteFile(
    name = "./data.csv",
    newIdent = testFileIdent
  )

  # Verify the change
  updatedRemoteFiles <- initialStepTemplate$stepDf$remoteFiles[[1]]
  updatedDataFile <- updatedRemoteFiles[updatedRemoteFiles$name == "./data.csv", ]

  cat("\n=== AFTER CHANGE ===\n")
  print(updatedRemoteFiles[, c("name", "asLink", "ident")])

  expect_equal(nrow(updatedDataFile), 1, info = "Should still have data.csv after change")
  expect_equal(updatedDataFile$ident, testFileIdent, info = "data.csv ident should be updated")
  expect_false(updatedDataFile$ident == originalIdent, info = "Ident should be different from original")

  cat("\n=== REALIZING WORKFLOW WITH CHANGED PARAMETER ===\n")

  # Create a new folder for the parameterized workflow
  paramFolder <- createFolder(TEST_FOLDER, "parameterized_dmg")
  workflowTemplate$setWorkflowTreeRootFolder(paramFolder$path)

  # Realize the workflow
  realizedWorkflow <- workflowTemplate$realise()

  expect_false(is.null(realizedWorkflow), info = "Should successfully realize workflow with changed parameter")

  cat("\n=== VERIFYING REALIZED WORKFLOW ===\n")

  # Check that the realized workflow has the changed file
  realizedDf <- realizedWorkflow$df()
  cat("Realized workflow has", nrow(realizedDf), "steps\n")

  # Find the realized Initial 1 step
  realizedInitialSteps <- realizedDf[realizedDf$description == "Initial 1", ]
  expect_equal(nrow(realizedInitialSteps), 1, info = "Should find 'Initial 1' in realized workflow")

  realizedInitialStepName <- realizedInitialSteps$fullName
  realizedInitialStep <- realizedWorkflow$steps[[realizedInitialStepName]]

  # Check the remoteFiles in the realized step
  realizedRemoteFiles <- realizedInitialStep$stepDf$remoteFiles[[1]]
  realizedDataFile <- realizedRemoteFiles[realizedRemoteFiles$name == "./data.csv", ]

  cat("\n=== REALIZED STEP REMOTE FILES ===\n")
  print(realizedRemoteFiles[, c("name", "asLink", "ident")])

  expect_equal(nrow(realizedDataFile), 1, info = "Should have data.csv in realized step")
  expect_equal(realizedDataFile$ident, testFileIdent, info = "Realized step should use new file ident")

  cat("\n✅ Parameterization test completed successfully!\n")
})


test_that("parameterize workflow using declarative API|ics1213,imr166", {
  TEST_FOLDER <- ensureTestFolder()

  cat("\n=== BUILDING DMG WORKFLOW FOR DECLARATIVE API TEST ===\n")

  # Create trees
  dmgL1 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "Param2 DMG L1")
  dmgL2 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "Param2 DMG L2")
  dmgL3 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "Param2 DMG L3")

  # Create workflow steps (simplified - just 3 steps)
  cat("Creating initial step...\n")
  i1 <- mockStep(dmgL1, paste0(TEST_FOLDER, "/data.csv"), "Initial 1", "data comes to system")

  cat("Creating processing step...\n")
  s1t1 <- mockStep(dmgL1, i1, "S1T1", "processing")

  cat("Creating final step...\n")
  s1t3 <- mockStep(dmgL3, s1t1, "S1T3", "report")

  cat("\n=== LOADING WORKFLOW WITH FULL LINEAGE ===\n")

  # Load the last step and its full lineage
  lastStep <- loadChildResources(dmgL3) %>% strip() %>% getStep()
  lastStep$lineage$load(stepDepth = -1, treeDepth = -1)

  # Create workflow template
  workflowTemplate <- lastStep$workflow$createTemplate()

  cat("\n=== EXAMINING WORKFLOW TEMPLATE ===\n")

  # Get all steps
  workflowDf <- workflowTemplate$df()
  cat("Workflow has", nrow(workflowDf), "steps\n")
  cat("Step descriptions:", paste(workflowDf$description, collapse=", "), "\n")

  # Get initial step info
  initialSteps <- workflowDf[workflowDf$description == "Initial 1", ]
  expect_equal(nrow(initialSteps), 1, info = "Should find exactly one 'Initial 1' step")

  initialStepName <- initialSteps$fullName
  cat("Initial step name:", initialStepName, "\n")

  initialStepTemplate <- workflowTemplate$stepTemplates[[initialStepName]]
  initialRemoteFiles <- initialStepTemplate$stepDf$remoteFiles[[1]]

  cat("\n=== INITIAL STEP REMOTE FILES (BEFORE PARAMETERIZATION) ===\n")
  print(initialRemoteFiles[, c("name", "asLink", "ident")])

  dataFile <- initialRemoteFiles[initialRemoteFiles$name == "./data.csv", ]
  originalIdent <- dataFile$ident
  cat("\nOriginal data.csv ident:", originalIdent, "\n")

  cat("\n=== USING NEW DECLARATIVE PARAMETERIZATION API ===\n")

  # Get the test file to use as new dataset
  testFileIdent <- loadResource(paste0(TEST_FOLDER, "/DataManipulation.Rmd"))$entityId
  cat("New test file ident:", testFileIdent, "\n")

  # Register parameter using new declarative API
  workflowTemplate$parameterizeStep(
    paramName = "inputDataset",
    stepPattern = "Initial 1",
    property = "remoteFile",
    target = "./data.csv",
    required = TRUE
  )

  cat("\n=== LISTING REGISTERED PARAMETERS ===\n")
  params <- workflowTemplate$listParameters()
  print(params)
  expect_equal(nrow(params), 1, info = "Should have 1 registered parameter")
  expect_equal(params$name, "inputDataset", info = "Parameter name should be inputDataset")
  expect_equal(params$required, TRUE, info = "Parameter should be required")
  expect_equal(params$applied, FALSE, info = "Parameter should not be applied yet")

  # Try to validate before setting value - should fail
  expect_error(
    workflowTemplate$validateParameters(),
    "Required parameters not set: inputDataset",
    info = "Should fail validation when required parameter not set"
  )

  # Set the parameter value
  workflowTemplate$setParameter("inputDataset", testFileIdent)

  cat("\n=== PARAMETERS AFTER SETTING VALUE ===\n")
  params <- workflowTemplate$listParameters()
  print(params)
  expect_equal(params$value, testFileIdent, info = "Parameter value should be updated")
  expect_equal(params$applied, FALSE, info = "Parameter should not be applied yet")

  # Now validation should pass
  expect_true(workflowTemplate$validateParameters(), info = "Parameters should validate after setting value")

  # Try to set a parameter that doesn't exist - should fail
  expect_error(
    workflowTemplate$setParameter("nonexistent", "value"),
    "Parameter 'nonexistent' has not been defined",
    info = "Should fail when setting undefined parameter"
  )

  cat("\n=== REALIZING WORKFLOW (PARAMETERS WILL BE APPLIED AUTOMATICALLY) ===\n")

  # Create a new folder for the parameterized workflow
  paramFolder <- createFolder(TEST_FOLDER, "parameterized_dmg2")
  workflowTemplate$setWorkflowTreeRootFolder(paramFolder$path)

  # Realize the workflow - parameters should be applied automatically
  realizedWorkflow <- workflowTemplate$realise()

  expect_false(is.null(realizedWorkflow), info = "Should successfully realize workflow")

  # Check that parameters were applied
  cat("\n=== VERIFYING PARAMETERS WERE APPLIED ===\n")
  params <- workflowTemplate$listParameters()
  print(params)
  expect_equal(params$applied, TRUE, info = "Parameter should be marked as applied after realization")

  cat("\n=== VERIFYING REALIZED WORKFLOW ===\n")

  # Check that the realized workflow has the changed file
  realizedDf <- realizedWorkflow$df()
  cat("Realized workflow has", nrow(realizedDf), "steps\n")

  # Find the realized Initial 1 step
  realizedInitialSteps <- realizedDf[realizedDf$description == "Initial 1", ]
  expect_equal(nrow(realizedInitialSteps), 1, info = "Should find 'Initial 1' in realized workflow")

  realizedInitialStepName <- realizedInitialSteps$fullName
  realizedInitialStep <- realizedWorkflow$steps[[realizedInitialStepName]]

  # Check the remoteFiles in the realized step
  realizedRemoteFiles <- realizedInitialStep$stepDf$remoteFiles[[1]]
  realizedDataFile <- realizedRemoteFiles[realizedRemoteFiles$name == "./data.csv", ]

  cat("\n=== REALIZED STEP REMOTE FILES ===\n")
  print(realizedRemoteFiles[, c("name", "asLink", "ident")])

  expect_equal(nrow(realizedDataFile), 1, info = "Should have data.csv in realized step")
  expect_equal(realizedDataFile$ident, testFileIdent, info = "Realized step should use new file ident")
  expect_false(realizedDataFile$ident == originalIdent, info = "Should be different from original")

  cat("\n✅ Declarative parameterization API test completed successfully!\n")
})


test_that("workflow template JSON serialization and deserialization|ics1213,imr166", {
  TEST_FOLDER <- ensureTestFolder()

  cat("\n=== BUILDING WORKFLOW FOR JSON TEST ===\n")

  # Create workflow with proper lineage (2 steps with dependency)
  dmgL1 <- createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "JSON DMG L1")

  cat("Creating steps with lineage...\n")
  i1 <- mockStep(dmgL1, paste0(TEST_FOLDER, "/data.csv"), "Initial JSON", "data input")
  s1 <- mockStep(dmgL1, i1, "Process JSON", "data processing")  # s1 uses i1's output

  # Load workflow - get last step and load its lineage
  allSteps <- loadFullChildResources(dmgL1) %>% strip()
  lastStep <- getStep(allSteps[allSteps$description=="Process JSON", ])  # Get the last step
  lastStep$lineage$load(stepDepth = -1, treeDepth = -1)

  workflowTemplate <- lastStep$workflow$createTemplate()

  cat("\n=== PARAMETERIZING WORKFLOW ===\n")

  # Add parameters
  testFileIdent <- loadResource(paste0(TEST_FOLDER, "/DataManipulation.Rmd"))$entityId

  workflowTemplate$parameterizeStep(
    paramName = "inputData",
    stepPattern = "Initial JSON",
    property = "remoteFile",
    target = "./data.csv",
    required = TRUE
  )

  workflowTemplate$parameterizeStep(
    paramName = "processingDesc",
    stepPattern = "Process JSON",
    property = "description",
    target = NULL,
    required = FALSE,
    defaultValue = "Default processing"
  )

  # Set one parameter, leave other unset
  workflowTemplate$setParameter("inputData", testFileIdent)

  cat("\n=== PARAMETERS BEFORE SAVE ===\n")
  paramsBefore <- workflowTemplate$listParameters()
  print(paramsBefore)

  expect_equal(nrow(paramsBefore), 2, info = "Should have 2 parameters")
  expect_equal(paramsBefore$name[1], "inputData", info = "First parameter should be inputData")
  expect_equal(paramsBefore$value[1], testFileIdent, info = "inputData should have value set")
  expect_equal(paramsBefore$name[2], "processingDesc", info = "Second parameter should be processingDesc")

  cat("\n=== SAVING TO JSON ===\n")

  jsonPath <- file.path("workflow_template.json")
  workflowTemplate$toJSON(jsonPath)

  expect_true(file.exists(jsonPath), info = "JSON file should be created")

  # Check JSON structure
  savedData <- jsonlite::read_json(jsonPath, simplifyVector = TRUE)

  # DIAGNOSTIC: Check what was saved in JSON
  cat("\n=== DIAGNOSTIC: JSON parameters ===\n")
  cat("Number of parameters saved:", nrow(savedData$parameters), "\n")
  for (i in 1:nrow(savedData$parameters)) {
    param <- savedData$parameters[i,]
    cat("\nParameter", i, ":\n")
    cat("  name:", param$name, "\n")
    cat("  value:", ifelse(is.null(param$value), "NULL", as.character(param$value)), "\n")
    cat("  defaultValue:", ifelse(is.null(param$defaultValue), "NULL", as.character(param$defaultValue)), "\n")
  }

  expect_equal(savedData$version, "1.0", info = "Should have version 1.0")
  expect_true(!is.null(savedData$parameters), info = "Should have parameters section")
  expect_true(!is.null(savedData$workflow), info = "Should have workflow section")
  expect_equal(nrow(savedData$workflow), 2, info = "Should have 2 steps in workflow")

  cat("\n=== LOADING FROM JSON ===\n")

  loadedTemplate <- workflowTemplateFromJSON(jsonPath)

  expect_false(is.null(loadedTemplate), info = "Should successfully load template")

  cat("\n=== PARAMETERS AFTER LOAD ===\n")
  paramsAfter <- loadedTemplate$listParameters()
  print(paramsAfter)

  # DIAGNOSTIC: Check what value[2] actually is
  cat("\n=== DIAGNOSTIC: processingDesc value ===\n")
  cat("paramsAfter$value[2]:", paramsAfter$value[2], "\n")
  cat("class:", class(paramsAfter$value[2]), "\n")
  cat("is.null:", is.null(paramsAfter$value[2]), "\n")
  cat("is.na:", is.na(paramsAfter$value[2]), "\n")
  cat("== '':", paramsAfter$value[2] == "", "\n")
  cat("nchar:", nchar(paramsAfter$value[2]), "\n")

  expect_equal(nrow(paramsAfter), 2, info = "Should have 2 parameters after load")
  expect_equal(paramsAfter$name[1], "inputData", info = "First parameter should be inputData")
  expect_equal(paramsAfter$value[1], testFileIdent, info = "inputData value should be restored")
  expect_equal(paramsAfter$name[2], "processingDesc", info = "Second parameter should be processingDesc")
  expect_true(paramsAfter$value[2] == "" || is.na(paramsAfter$value[2]), info = "processingDesc should have no value")

  cat("\n=== VERIFYING WORKFLOW STRUCTURE ===\n")

  loadedDf <- loadedTemplate$df()
  originalDf <- workflowTemplate$df()

  expect_equal(nrow(loadedDf), nrow(originalDf), info = "Should have same number of steps")
  expect_equal(loadedDf$description, originalDf$description, info = "Step descriptions should match")

  cat("\n=== TESTING LOADED TEMPLATE FUNCTIONALITY ===\n")

  # Set the second parameter and validate
  loadedTemplate$setParameter("processingDesc", "Custom processing")

  expect_true(loadedTemplate$validateParameters(), info = "Should validate after setting parameters")

  paramsAfterSet <- loadedTemplate$listParameters()
  expect_equal(paramsAfterSet$value[2], "Custom processing", info = "processingDesc should be updated")

  cat("\n=== REALIZING LOADED TEMPLATE ===\n")

  # Create folder and realize
  jsonFolder <- createFolder(TEST_FOLDER, "json_workflow")
  loadedTemplate$setWorkflowTreeRootFolder(jsonFolder$path)

  realizedWorkflow <- loadedTemplate$realise()

  expect_false(is.null(realizedWorkflow), info = "Should successfully realize loaded template")

  # Verify parameters were applied
  paramsApplied <- loadedTemplate$listParameters()
  expect_true(all(paramsApplied$applied), info = "All parameters should be applied after realization")

  cat("\n=== VERIFYING REALIZED WORKFLOW ===\n")

  realizedDf <- realizedWorkflow$df()
  expect_equal(nrow(realizedDf), 2, info = "Realized workflow should have 2 steps")

  # Check that inputData parameter was applied
  initialStep <- realizedDf[realizedDf$description == "Initial JSON", ]
  initialStepEnv <- realizedWorkflow$steps[[initialStep$fullName]]
  initialRemoteFiles <- initialStepEnv$stepDf$remoteFiles[[1]]
  dataFile <- initialRemoteFiles[initialRemoteFiles$name == "./data.csv", ]

  expect_equal(dataFile$ident, testFileIdent, info = "Data file should use parameterized ident")

  # Check that description parameter was applied
  processStep <- realizedDf[realizedDf$description == "Custom processing", ]
  expect_equal(nrow(processStep), 1, info = "Should find step with custom description")

  cat("\n✅ JSON serialization/deserialization test completed successfully!\n")
})


test_that("parameterize workflow with local files|ics1213,imr166", {
  TEST_FOLDER <- ensureTestFolder()

  cat("\n=== Creating test tree ===\n")
  testTree <- improveR::createAnalysisTree(
    targetIdent = TEST_FOLDER,
    treeName = "LocalFile Param Test"
  )

  cat("\n=== Creating step template with local files ===\n")

  # Create test files
  testFile1 <- tempfile(fileext = ".txt")
  testFile2 <- tempfile(fileext = ".txt")
  writeLines("test content 1", testFile1)
  writeLines("test content 2", testFile2)

  # Create step template
  stepTemplate <- improveR::createStepTemplateEnv(treeIdent = testTree$resourceId)
  stepTemplate$setStepRunserverLabel(Sys.getenv("R_RUNSERVER"))
  stepTemplate$setStepToolLabel(Sys.getenv("R_TOOL"))
  stepTemplate$setStepToolInstance(Sys.getenv("R_TOOL_INSTANCE"))
  stepTemplate$setStepDescription("Local File Step")
  stepTemplate$setStepRationale("Test local file parameterization")

  # Add local files - one as command-file variable
  stepTemplate$addStepLocalFile(testFile1, name = "./input.txt", variableName = "command-file")
  stepTemplate$addStepLocalFile(testFile2, name = "./config.txt")

  cat("\n=== Creating workflow template ===\n")

  # Create empty workflow template and add step
  workflowTemplate <- improveR::createWorkflowTemplateEnv()
  workflowTemplate$addStepTemplate(stepTemplate)

  # Verify step was added
  workflowDf <- workflowTemplate$df()
  expect_equal(nrow(workflowDf), 1)

  cat("\n=== Parameterizing local files ===\n")

  # Parameterize the local files
  workflowTemplate$parameterizeStep(
    paramName = "inputFile",
    stepPattern = "*",
    property = "localFile",
    target = "./input.txt",
    required = TRUE
  )

  # Verify parameters registered
  params <- workflowTemplate$listParameters()
  cat("Parameters:\n")
  print(params)
  expect_equal(nrow(params), 1)

  cat("\n=== Setting parameter values ===\n")

  # Create new test file for parameter
  newTestFile <- tempfile(fileext = ".txt")
  writeLines("new test content", newTestFile)

  workflowTemplate$setParameter("inputFile", newTestFile)

  # Verify parameter value set
  params <- workflowTemplate$listParameters()
  expect_equal(params[params$name == "inputFile", ]$value, newTestFile)

  cat("\n=== Saving to JSON ===\n")

  templatePath <- file.path(tempdir(), "localfile_template.json")
  workflowTemplate$toJSON(templatePath)
  expect_true(file.exists(templatePath))

  # Check JSON content
  savedData <- jsonlite::read_json(templatePath, simplifyVector = TRUE)
  expect_equal(nrow(savedData$parameters), 1)
  expect_equal(savedData$parameters$property[1], "localFile")

  cat("\n=== Loading from JSON ===\n")

  loadedTemplate <- improveR::workflowTemplateFromJSON(templatePath)
  expect_false(is.null(loadedTemplate))

  # Verify parameters loaded
  loadedParams <- loadedTemplate$listParameters()
  expect_equal(nrow(loadedParams), 1)
  expect_equal(loadedParams$name[1], "inputFile")

  cat("\n=== Setting parameter on loaded template ===\n")

  # Set the parameter value
  loadedTemplate$setParameter("inputFile", newTestFile)

  cat("\n=== Realizing workflow ===\n")

  # Set workflow tree and realize
  loadedTemplate$setWorkflowTreeIdent(testTree$resourceId)
  loadedTemplate$validateParameters()
  workflow <- loadedTemplate$realise()

  # Verify the workflow was created
  expect_false(is.null(workflow))
  expect_true(is.environment(workflow))

  cat("\n✅ Local file parameterization test completed successfully!\n")

  # Cleanup
  unlink(testFile1)
  unlink(testFile2)
  unlink(newTestFile)
})
