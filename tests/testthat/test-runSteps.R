Sys.setenv(TEST_NAME="runSteps")

# Helper function to ensure TEST_FOLDER exists when running tests individually
ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME="runSteps")
  if (!exists("TEST_FOLDER") || is.null(TEST_FOLDER)) {
    improveR::setEditable(TRUE)
    TEST_FOLDER <- improveR:::workflowFilesSetup()
    assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
    return(TEST_FOLDER)
  }
  return(TEST_FOLDER)
}

#httptest::with_mock_dir("prepare-runSteps",{
  test_that("createTestFolder", {
    cat("\n=== CREATE TEST FOLDER TEST ===\n")
    Sys.setenv(IMPROVER_TEST_REPLAY="T")

    # Try to connect if not already connected
    if (!improveR::improveConnected()) {
      cat("Not connected, attempting connection...\n")
      tryCatch({
        improveConnect()
      }, error = function(e) {
        cat("Connection failed in test:", e$message, "\n")
        cat("Checking if we have a token anyway...\n")
      })
    } else {
      cat("Already connected\n")
    }

    setEditable(T)

    # Check token status
    has_token <- Sys.getenv("IMPROVER_TOKEN") != ""
    cat("Has token:", has_token, "\n")
    if (!has_token) {
      cat("WARNING: No token found, test may fail\n")
    }
    expect_false(Sys.getenv("IMPROVER_TOKEN")=="")

    cat("Setting up workflow files...\n")
    TEST_FOLDER <- improveR:::workflowFilesSetup()
    cat("TEST_FOLDER created:", TEST_FOLDER, "\n")
    assign(x = "TEST_FOLDER",value = TEST_FOLDER,envir = globalenv())
  })
#})


library(magrittr)

#httptest::with_mock_dir("checkRunservers", {
  test_that("check runservers|ics1216,ics1226,ics1227,ics1229,ics1230", {
    nonmem_runserver <- Sys.getenv("NONMEM_RUNSERVER")
    nonmem_tool <- Sys.getenv("NONMEM_TOOL")
    nonmem_tool_instance <- Sys.getenv("NONMEM_TOOL_INSTANCE")

    r_runserver <- Sys.getenv("R_RUNSERVER")
    r_tool <- Sys.getenv("R_TOOL")
    r_tool_instance <- Sys.getenv("R_TOOL_INSTANCE")

    runserver <- loadRunserver(r_runserver)
    expect_equal(1, nrow(runserver))

    runserverTool <- loadToolForRunserver(runserver$id, r_tool, r_tool_instance)
    expect_equal(1, nrow(runserverTool))

    runserver <- loadRunserver(nonmem_runserver)
    expect_equal(1, nrow(runserver))

    runserverTool <- loadToolForRunserver(runserver$id, nonmem_tool, nonmem_tool_instance)
    expect_equal(1, nrow(runserverTool))

    gridProvider <- runserverTool$gridProvider

    gridArgumentDefinitions <- loadGridArguments(gridProvider)

    queue <- gridArgumentDefinitions[gridArgumentDefinitions$name == "queue", ]
    expect_equal(nrow(queue), 1)
    expect_equal(queue$gridArgumentType, "LOV")
    values <- queue$category[[1]]$values[[1]]
    short <- values[values$text == "short", ]
    expect_equal(nrow(short), 1)
    long <- values[values$text == "priority", ]
    expect_equal(nrow(long), 1)

    cores <- gridArgumentDefinitions[gridArgumentDefinitions$name == "cores", ]
    expect_equal(nrow(cores), 1)
    expect_equal(cores$gridArgumentType, "TEXT")

    start <- gridArgumentDefinitions[gridArgumentDefinitions$name == "start", ]
    expect_equal(nrow(start), 1)
    expect_equal(start$gridArgumentType, "DATE_TIME")

    empty <- gridArgumentDefinitions[gridArgumentDefinitions$name == "empty", ]
    expect_equal(nrow(empty), 1)
    expect_equal(empty$gridArgumentType, "EMPTY")
  })
#})

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

#httptest::with_mock_dir("loadChildSteps", {
  test_that("load Child steps|ics1140,ics1205,ics1209,ics1225", {
    # Ensure TEST_FOLDER exists (for when test is run individually)
    TEST_FOLDER <- ensureTestFolder()

    # Create tree
    testTree <- createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "ChildSteps")

    # Create root step
    stepEnv <- rBatchStep(testTree)
    stepEnv$setStepDescription("Data Manipulation")
    stepEnv$setStepRationale("to manipulate data")
    stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"), variableName = "command-file")
    stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.Rmd"))
    stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/data.csv"))

    stepEnv$realise()

    rootStep <- stepEnv$getStepResource()

    # Create child1
    stepEnv <- rBatchStep(testTree)
    stepEnv$setStepParent(rootStep$resourceId)
    stepEnv$setStepDescription("Data Manipulation")
    stepEnv$setStepRationale("to manipulate data")
    stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"), variableName = "command-file")
    stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.Rmd"))
    stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/data.csv"))
    stepEnv$realise()

    child1 <- stepEnv$getStepResource()

    # Create grandchild one
    stepEnv <- rBatchStep(testTree)
      stepEnv$setStepParent(child1$resourceId)
      stepEnv$setStepDescription("Data Manipulation")
      stepEnv$setStepRationale("to manipulate data")
      stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"), variableName = "command-file")
      stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.Rmd"))
      stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/data.csv"))
      stepEnv$realise()

    child11 <- stepEnv$getStepResource()

    # Create grandchild 2
    stepEnv <- rBatchStep(testTree)
    stepEnv$setStepParent(child1$resourceId)
    stepEnv$setStepDescription("Data Manipulation")
    stepEnv$setStepRationale("to manipulate data")
    stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"), variableName = "command-file")
    stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.Rmd"))
    stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/data.csv"))
    stepEnv$realise()

    child12 <- stepEnv$getStepResource()

    # Load child steps tree
    childStepsTree <- loadChildSteps(testTree)
    expect_null(childStepsTree)

    # Child is child of root
    childStepsRootSteps <- loadChildSteps(rootStep)
    expect_equal(1, nrow(childStepsRootSteps))
    childStepsRootSteps <- childStepsRootSteps$data[[1]]
    expect_equal(1, nrow(childStepsRootSteps))
    expect_true(child1$resourceId %in% childStepsRootSteps$resourceId)

    # Grandchildren children of child
    childStepsRootSteps <- loadChildSteps(child1)
    expect_equal(1, nrow(childStepsRootSteps))
    childStepsRootSteps <- childStepsRootSteps$data[[1]]
    expect_equal(2, nrow(childStepsRootSteps))
    expect_true(child11$resourceId %in% childStepsRootSteps$resourceId)
    expect_true(child12$resourceId %in% childStepsRootSteps$resourceId)

    # Grandchildren do not have children
    children <- rbind(child11, child12)
    childStepsRootSteps <- loadChildSteps(children)
    expect_equal(2, nrow(childStepsRootSteps))
    childStepsRootSteps <- childStepsRootSteps$data[[1]]
    expect_equal(0, nrow(childStepsRootSteps))

    # Back up to root
    parentStep <- loadParentStep(child12)
    expect_equal(parentStep$resourceId, child1$resourceId)
    parentStep <- loadParentStep(child1)
    expect_equal(parentStep$resourceId, rootStep$resourceId)
    parentStep <- loadParentStep(rootStep)
    expect_null(parentStep)

    # Disconnect all
    detachStep(child1)
    detachStep(child11)
    detachStep(child12)

    # Check all disconnected
    parentStep <- loadParentStep(child12)
    expect_null(parentStep)
    parentStep <- loadParentStep(child11)
    expect_null(parentStep)
    parentStep <- loadParentStep(child1)
    expect_null(parentStep)
    parentStep <- loadParentStep(rootStep)
    expect_null(parentStep)

    # Reconnect other way round
    attachStep(rootStep, child12)
    attachStep(child1, child12)
    attachStep(child12, child11)

    # Check reconnect
    parentStep <- loadParentStep(child12)
    expect_equal(parentStep$resourceId, child11$resourceId)
    parentStep <- loadParentStep(child1)
    expect_equal(parentStep$resourceId, child12$resourceId)
    parentStep <- loadParentStep(rootStep)
    expect_equal(parentStep$resourceId, child12$resourceId)
    parentStep <- loadParentStep(child11)
    expect_null(parentStep)

    })
#})


test_that("subfolder in step inventory|ics1140,ics1213,ics1214", {
  # Ensure TEST_FOLDER exists (for when test is run individually)
  TEST_FOLDER <- ensureTestFolder()

  testTree <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "subfolderInventory")

  stepEnv <- rBatchStep(testTree)
  stepEnv$setStepDescription("Data Manipulation")
  stepEnv$setStepRationale("to manipulate data")
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"), variableName = "command-file")
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.Rmd"), name = "subfolder/test.Rmd", asLink = F)
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/data.csv"))
  step <- stepEnv$realise()
  stepEnv$finishRun()


  inventory <- step$getStepInventory( recurse = T) %>%
    improveR::strip() %>%
    dplyr::filter(name == "test.Rmd")

  expect_equal(nrow(inventory), 1)
  expect_equal(inventory$path,
    file.path(step$getStepResource()$path,
      "subfolder",
      inventory$name))

  checkFile <- improveR::getFile(inventory)
  expect_true(file.exists(checkFile$path))
  expect_equal(file.info(checkFile$path)$size, 1387)

  # Link file from subfolder in workflow
  stepEnv <- rBatchStep(testTree)
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/EDA.R"), variableName = "command-file")
  stepEnv$addStepRemoteFile(inventory, name = "data.csv")
  stepEnv$realise()
  stepEnv$finishRun()

  # Check update to subfolder file
###################here
  retryFlow <- getWorkflow(testTree)
  retryFlowDf <- retryFlow$df()
  updateFileStep <- dplyr::filter(retryFlowDf, description == "Data Manipulation") %>%
    dplyr::pull(fullName)

  # Handle case where multiple steps might exist with same description
  if (length(updateFileStep) > 1) {
    # Take the most recent one (last in the list)
    updateFileStep <- updateFileStep[length(updateFileStep)]
  }

  expect_equal(length(updateFileStep), 1,
               info = "Should find exactly one 'Data Manipulation' step")

  updateFileStepEnv <- retryFlow$steps[[updateFileStep]]

  updateFileStepEnv$getStepInventory(recurse = T) %>%
    improveR::strip() %>%
    dplyr::filter(inventoryPath == "subfolder/test.Rmd") %>%
    improveR::updateFileContent(localPath = "improver.log")

  #TODO rerun, checkin include pattern for input files
  retryFlow$rerunChangedAndOutdated()




#ask for working copies?
  #ticket, add changed flag to dmg

  retryFlow <- getWorkflow(testTree)
  retryTemplate <- retryFlow$createTemplate()
  retryTemplate$realise()
})

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

test_that("DMG spans multiple trees, linear|ics1140", {
  cat("\n=== DMG TEST STARTING ===\n")
  # Ensure TEST_FOLDER exists (for when test is run individually)
  TEST_FOLDER <- ensureTestFolder()
  cat("TEST_FOLDER:", TEST_FOLDER, "\n")

  cat("Creating analysis trees...\n")
  dmgL1 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "DMG L1")
  cat("Created DMG L1\n")
  dmgL2 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "DMG L2")
  cat("Created DMG L2\n")
  dmgL3 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "DMG L3")
  cat("Created DMG L3\n")

  cat("Creating initial mock steps...\n")
  i1 <- mockStep(dmgL1, paste0(TEST_FOLDER, "/data.csv"), "Initial 1", "data comes to system")
  cat("Created i1\n")
  i2 <- mockStep(dmgL1, paste0(TEST_FOLDER, "/data.csv"), "Initial 2", "data comes to system")
  cat("Created i2\n")

  cat("Creating s1t1...\n")
  s1t1 <- mockStep(dmgL1, i1, "S1T1", "processing", dataSet2 = i2)
  cat("Created s1t1\n")

  cat("Creating s2t1...\n")
  s2t1 <- mockStep(dmgL1, s1t1, "S2T1", "processing", dataSet2 = i2)
  cat("Created s2t1\n")

  cat("Creating s1t2...\n")
  s1t2 <- mockStep(dmgL2, s1t1, "S1T2", "processing", dataSet2 = s2t1)
  cat("Created s1t2\n")

  cat("Creating s2t2...\n")
  s2t2 <- mockStep(dmgL2, s1t2, "S2T2", "processing", dataSet2 = i2)
  cat("Created s2t2\n")

  cat("Creating s1t3...\n")
  s1t3 <- mockStep(dmgL3, s1t2, "S1T3", "report", dataSet2 = s2t2)
  cat("Created s1t3\n")
  #just test the import
  #TODO remove this check

  cat("Creating fullLineage folder...\n")
  fullLineageFolder <- createFolder(TEST_FOLDER,"fullLineage")
  cat("Created fullLineage folder\n")

  cat("Loading last step from dmgL3...\n")
  lastStep <- loadChildResources(dmgL3) %>%strip() %>% getStep()
  cat("Loaded last step\n")
  
  cat("Loading lineage with stepDepth=-1, treeDepth=-1...\n")
  lastStep$lineage$load(stepDepth = -1,treeDepth = -1)
  cat("Loaded lineage\n")
  
  cat("Creating workflow template...\n")
  fullLineageTemplate <- lastStep$workflow$createTemplate()
  cat("Created template\n")
  
  cat("Setting workflow tree root folder...\n")
  fullLineageTemplate$setWorkflowTreeRootFolder(fullLineageFolder$path)
  cat("Set root folder\n")
  
  cat("Realising template...\n")
  lineageResult <- fullLineageTemplate$realise()
  cat("Template realised\n")



  expect_equal(nrow(loadChildResources(fullLineageFolder)$data[[1]]),3)
  expect_equal(nrow(loadChildResources("./DMG L1",fullLineageFolder)$data[[1]]),4)
  expect_equal(nrow(loadChildResources("./DMG L2",fullLineageFolder)$data[[1]]),2)
  expect_equal(nrow(loadChildResources("./DMG L3",fullLineageFolder)$data[[1]]),1)


  lastStep <- loadChildResources(dmgL3) %>%strip() %>% getStep()
  lastStep$lineage$load(stepDepth = -1,treeDepth = 1)
  fullLineageTemplate <- lastStep$workflow$createTemplate()
  fullLineageTemplate$setWorkflowTreeRootFolder(fullLineageFolder$path)
  fullLineageTemplate$setWorkflowTreeName("AllInOne")
  lineageResult <- fullLineageTemplate$realise()



  expect_equal(nrow(loadChildResources(fullLineageFolder)$data[[1]]),4)
  expect_equal(nrow(loadChildResources("./AllInOne",fullLineageFolder)$data[[1]]),3)

  fullUsageFolder <- createFolder(TEST_FOLDER,"fullUsage")

  firstStep <- loadFullChildResources(dmgL1)%>%strip() %>%
    dplyr::filter(description=="Initial 2")%>%getStep()
  firstStep$usage$load(stepDepth = -1,treeDepth = -1)
  firstStepTemplate<-firstStep$workflow$createTemplate()
  firstStepTemplate$setWorkflowTreeRootFolder(fullUsageFolder$path)
  firstStepReexec <- firstStepTemplate$realise()



  expect_equal(nrow(loadChildResources(fullUsageFolder)$data[[1]]),4)
  expect_equal(nrow(loadChildResources("./DMG L1",fullUsageFolder)$data[[1]]),3)
  expect_equal(nrow(loadChildResources("./DMG L2",fullUsageFolder)$data[[1]]),2)
  expect_equal(nrow(loadChildResources("./DMG L3",fullUsageFolder)$data[[1]]),1)



  firstStep <- loadFullChildResources(dmgL1)%>%strip() %>%
    dplyr::filter(description=="Initial 2")%>%getStep()
  firstStep$usage$load(stepDepth = -1,treeDepth = 1)
  firstStepTemplate<-firstStep$workflow$createTemplate()
  firstStepTemplate$setWorkflowTreeRootFolder(fullUsageFolder$path)
  firstStepTemplate$setWorkflowTreeName("AllInOne")
  firstStepReexec <- firstStepTemplate$realise()


  expect_equal(nrow(loadChildResources(fullUsageFolder)$data[[1]]),4)
  expect_equal(nrow(loadChildResources("./AllInOne",fullUsageFolder)$data[[1]]),10)


   lineageWorkflowStep <- loadChildResources(dmgL3)%>%strip() %>%
     getStep()
   lineageWorkflowStep$lineage$load(stepDepth = -1,treeDepth = -1)

   lineageWorkflowStep$workflow$createTemplate()$realise()

   expect_equal(nrow(loadChildResources("./DMG L1",TEST_FOLDER)$data[[1]]),8)
   expect_equal(nrow(loadChildResources("./DMG L2",TEST_FOLDER)$data[[1]]),4)
   expect_equal(nrow(loadChildResources("./DMG L3",TEST_FOLDER)$data[[1]]),2)



  report <- loadChildResources(dmgL3)%>%strip()
  reportStep <- getStep(report[1,])
  reportStep$lineage$load(stepDepth = -1,treeDepth = -1)
  # exportWorkflow now expects a workflow, not a workflow template
  workflow <- reportStep$workflow

  # Export workflow and verify file creation
  cat("\n=== EXPORT/IMPORT TEST ===\n")
  cat("Exporting workflow to lineageDMG.zip...\n")
  exportWorkflow(workflow,workflowName = "lineageDMG")

  # Check export file was created
  expect_true(file.exists("lineageDMG.zip"),
              info = "Export file lineageDMG.zip should be created")
  cat("Export file created: lineageDMG.zip\n")
  cat("Export file size:", file.info("lineageDMG.zip")$size, "bytes\n")

  # Check mapping files were created
  expect_true(file.exists("lineageDMGLinkMapping.json"),
              info = "Link mapping file should be created")
  expect_true(file.exists("lineageDMGToolMapping.json"),
              info = "Tool mapping file should be created")
  cat("Mapping files created\n")

  # Import workflow
  importRepoFolder <- file.path(TEST_FOLDER,"import1")
  cat("Creating import folder:", importRepoFolder, "\n")
  createFolder(dirname(importRepoFolder),basename(importRepoFolder))

  cat("Importing workflow from lineageDMG.zip...\n")
  importWorkflow("lineageDMG.zip",importRepoFolder)
  cat("Import completed\n")

  # Verify import created the expected tree structure
  cat("Checking imported tree structure...\n")
  importedResources <- loadChildResources(importRepoFolder)
  expect_false(is.null(importedResources),
               info = "Imported folder should contain resources")

  if (!is.null(importedResources) && !is.null(importedResources$data[[1]])) {
    importedTrees <- importedResources$data[[1]]
    cat("Found", nrow(importedTrees), "trees in import folder\n")
    cat("Imported tree names:", paste(importedTrees$name, collapse=", "), "\n")

    # Check that DMG trees were imported
    expect_true(any(grepl("DMG", importedTrees$name)),
                info = "Should have imported DMG trees")
  } else {
    cat("WARNING: No trees found in import folder\n")
  }

  # Clean up export files
  cat("Cleaning up export files...\n")
  if (file.exists("lineageDMG.zip")) unlink("lineageDMG.zip")
  if (file.exists("lineageDMGLinkMapping.json")) unlink("lineageDMGLinkMapping.json")
  if (file.exists("lineageDMGToolMapping.json")) unlink("lineageDMGToolMapping.json")
  #externalLinkMapping





  #test externalLinks
  #test with subfolders, test with also inputfiles
  #integrate cache
  #zip handling and tempfolderHandling
#mapping of input file variables
  #parental relations in workflows



})

test_that("simple nonmem step with all grid combinations|ics1140,ics1222,ics1213", {
  TEST_FOLDER <- ensureTestFolder()


  testTree <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "SimpleNonmem")

  nonmemStep <- nonmemBatchStep(testTree)
  nonmemStep$addStepRemoteFile(paste0(TEST_FOLDER, "/STEP1.ctl"), variableName = "command-file")
  #nonmemStep$addStepRemoteFile(paste0(TEST_FOLDER, "/STEP1.ctl"), name = "folder/file.txt", variableName = "command-file")
  nonmemStep$addStepRemoteFile(paste0(TEST_FOLDER, "/example-new.dat"), variableName = "dataset")
  nonmemStep$setStepDescription("description1")
  nonmemStep$setStepRationale("rational")
  nonmemStep$realise()
  nonmemStep$finishRun()

  dN <- getCopy(paste0(TEST_FOLDER, "/STEP1.ctl"))

  nonmemStep2 <- nonmemBatchStep(testTree)
  nonmemStep2$addStepLocalFile(dN$path, variableName = "command-file")
  nonmemStep2$addStepRemoteFile(paste0(TEST_FOLDER, "/example-new.dat"), variableName = "dataset")
  nonmemStep2$setStepDescription("I am showing a nonmem step")
  nonmemStep2$setStepRationale("so you have seen it")
  realStep <- nonmemStep2$realise()
  nonmemStep2$finishRun()

  inventory <- realStep$getStepInventory()
  inv <- inventory$data[[1]]
  inv <- inv[inv$name == "output.txt", ]

  invCopy <- improveR::getCopy(inv)

  unlink(dN$path)
  unlink(invCopy$path)


  step <- realStep$getStepResource()
  step <- improveR::updateResource(step)
  expect_equal(realStep$getStepState(), "FINISHED")
  entityId <- step$entityId

  process <- improveR::loadProcessesForStep(entityId)

  # Create step copy
  copyTemplate <- realStep$workflow$createTemplate()
  testTree2 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "SimpleNonmemCopy")
  copyTemplate$stepTemplates[[1]]$setStepTree(testTree2$resourceId)
  copyWorkflow <- copyTemplate$realise()




  result <- improveR::deleteGridArgumentsByName(process$id, "queue")
  result <- improveR::deleteGridArgumentsByName(process$id, "cores")
  result <- improveR::deleteGridArgumentsByName(process$id, "start")
  result <- improveR::deleteGridArgumentsByName(process$id, "empty")
  result <- improveR::deleteGridArgumentsByName(process$id, "stderr")
  result <- improveR::deleteGridArgumentsByName(process$id, "stdout")

  expect_equal(result, NULL)

  result <- improveR::setGridArgument(process$id, "queue", "priority", update = T)
  expect_equal(nrow(result), 1)
  result <- improveR::setGridArgument(process$id, "queue", "short", update = T)
  expect_equal(nrow(result), 1)
  values <- result$category[[1]]$values[[1]]
  expect_equal("short", values[values$id == result$lovValueId, ]$text)

  result <- improveR::setGridArgument(process$id, "cores", "4", update = T)
  expect_equal(nrow(result), 2)
  result <- improveR::setGridArgument(process$id, "cores", "5", update = T)
  expect_equal(nrow(result), 2)
  result <- result[result$name == "cores", ]
  expect_equal(result$textValue, "5")

  result <- improveR::setGridArgument(process$id, "start", Sys.time(), update = T)
  expect_equal(nrow(result), 3)
  newTime <- Sys.time()
  result <- improveR::setGridArgument(process$id, "start", newTime, update = T)
  expect_equal(nrow(result), 3)
  result <- result[result$name == "start", ]
  expect_equal(
    substr(x = as.character(improveR:::convertImproveTimestampToPosix(result$dateValue)), 0, 14),
    substr(x = as.character(newTime), 0, 14))

  result <- improveR::setGridArgument(process$id, "empty", "", update = T)
  expect_equal(nrow(result), 4)

  # Empty grid values
  checkgridFlow <- getWorkflow(testTree)
  testTree3 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "SimpleNonmemCopy Grid Check")


  checkGridTemplate <- checkgridFlow$createTemplate()
  checkGridTemplate$setWorkflowTreeIdent(testTree3)
  compareFlow <- checkGridTemplate$realise()


  #TODO grid arguments, grid arguments merging
  args <- dplyr::filter(checkgridFlow$df(), description == "I am showing a nonmem step")$processes[[1]]$gridArguments[[1]] %>% dplyr::select("argumentName", "argumentValue")
  argsCompare <- dplyr::filter(compareFlow$df(), description == "I am showing a nonmem step")$processes[[1]]$gridArguments[[1]] %>% dplyr::select("argumentName", "argumentValue")
  expect_equal(args, argsCompare)
  expect_equal(nrow(args), 4)
})

test_that("test full workflow|ics1140,ics1211,ics1212,ics1213,ics1214,ics1220", {
  TEST_FOLDER <- ensureTestFolder()
  # Skip workflow import tests in version 4.3 due to compatibility issues
  # repoVersion <- getRepositoryVersion()
  # if (!is.null(repoVersion)) {
  #   versionParts <- strsplit(repoVersion, "[.-]")[[1]]
  #   if (length(versionParts) >= 2) {
  #     majorMinor <- as.numeric(paste0(versionParts[1], ".", versionParts[2]))
  #     if (majorMinor < 4.4) {
  #       skip("Skipping workflow import tests in repository version < 4.4")
  #     }
  #   }
  # }

  testTree <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "SimpleWorkflow")

  dmTemplate  <- rBatchStep(testTree)
  dmTemplate$setStepDescription("Data Manipulation")
  dmTemplate$setStepRationale("to manipulate data")
  dmTemplate$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"), variableName = "command-file")
  dmTemplate$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.Rmd"))
  dmTemplate$addStepRemoteFile(paste0(TEST_FOLDER, "/data.csv"))

  dmStep <- dmTemplate$realise()
  dmTemplate$finishRun()

  dmStep <- dmTemplate$realise()
  dmTemplate$finishRun()

  expect_equal(dmStep$getStepState(), "FINISHED")

  inventory <- dmStep$getStepInventory()$data[[1]]

  cleanDataset <- inventory[inventory$name == "chapter15_example_cleaned.rds", ]

  edaTemplate <- rBatchStep(testTree)
  edaTemplate$addStepRemoteFile(paste0(TEST_FOLDER, "/EDA.R"), variableName = "command-file")
  edaTemplate$addStepRemoteFile(cleanDataset)
  edaStep <- edaTemplate$realise()

  lmTemplate <- rBatchStep(testTree)
  lmTemplate$addStepRemoteFile(paste0(TEST_FOLDER, "/test_lm_plot.R"), variableName = "command-file")
  lmTemplate$addStepRemoteFile(cleanDataset)
  lmStep <- lmTemplate$realise()

  lmTemplate$finishRun()
  edaTemplate$finishRun()

  inventory <- edaStep$getStepInventory()$data[[1]]
  byDay <- inventory[inventory$name == "HAMDTL17_by_day.png", ]
  byWeek <- inventory[inventory$name == "HAMDTL17_by_week_therapy.png", ]
  edaTable <- inventory[inventory$name == "EDA_table.html", ]
  inventoryLM <- lmStep$getStepInventory()$data[[1]]
  modelFit <- inventoryLM[inventoryLM$name == "model_fit.html", ]

  reportTemplate <- rBatchStep(testTree)
  reportTemplate$addStepRemoteFile(paste0(TEST_FOLDER, "/report.R"), variableName = "command-file")
  reportTemplate$addStepRemoteFile(paste0(TEST_FOLDER, "/report.Rmd"))
  reportTemplate$addStepRemoteFile(byDay)
  reportTemplate$addStepRemoteFile(byWeek)
  reportTemplate$addStepRemoteFile(edaTable)
  reportTemplate$addStepRemoteFile(modelFit)
  reportStep <- reportTemplate$realise()
  reportTemplate$finishRun()

  # Reexecute in new tree
  workflow <- getWorkflow(testTree)
  testTree2 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "SimpleWorkflow Reexecute")
  reexecuteTemplate <- workflow$createTemplate(addParental = T)
  reexecuteTemplate$setWorkflowTreeIdent(testTree2)
  reexecuteTemplate$realise()


  # Reexecute in same tree
  # With parental

  workflow <- getWorkflow(testTree2)
  reexecuteTemplate <- workflow$createTemplate(addParental = T)
  reexecuteTemplate$realise()


  # Without parental
  reexecuteTemplate <- workflow$createTemplate(addParental = F)
  reexecuteTemplate$realise()
#########################################################################
  # Reexecute in same tree, keep part absolute
  reexecuteTemplate <- workflow$createTemplate(addParental = F)
  testTree3 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "SimpleWorkflow Reexecute Partial")
  reexecuteTemplate$setWorkflowTreeIdent(testTree3)
  reexecuteTemplate$realise()

  partial <- getWorkflow(testTree3)
  stepsInpartial <- names(partial$steps)
  stepsInpartial<-stepsInpartial[!grepl(pattern = "Step 5",x=stepsInpartial) & !grepl(pattern = "Step 4",x=stepsInpartial)]
  x<-lapply(stepsInpartial,function(removeStep) {
    partial$removeStep(partial$steps[[removeStep]])
  })
  partialTemplate <- partial$createTemplate()

  partialTemplate$realise()


  # Reexecute outdated
  testTree4 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "SimpleWorkflow Reexecute Outdated and Full")
  full <- getWorkflow(testTree)
  fullTemplate <- full$createTemplate()
  fullTemplate$setWorkflowTreeIdent(testTree4)

  dmName <- names(fullTemplate$stepTemplates)[grep(pattern = "Step 2", names(fullTemplate$stepTemplates))]
  dmStep <- fullTemplate$stepTemplates[[dmName]]


  # Change data.csv to add as copy
  dmStep$changeStepRemoteFile("./data.csv", asLink = F)

  test4Workflow <- fullTemplate$realise()
  test4Workflow <- getWorkflow(testTree4)
  dmReal <- test4Workflow$steps[[names(test4Workflow$steps)[grep(pattern = "Step 2", names(test4Workflow$steps))]]]

  inventory <- dmReal$getStepInventory()$data[[1]]
  inputFile <- inventory[inventory$name == "data.csv", ]
  if (!file.exists("improver.log")) {
    file.create("improver.log")
  }
  improveR::updateFileContent(inputFile, "improver.log")
  test4Workflow$rerunChangedAndOutdated()

  executedSteps <- improveR::loadChildResources(testTree4)$data[[1]]

  relativeWorkflowAfter <- improveR::byNotEmptyAsDf(executedSteps, function(line) {
    process <- improveR::getMainProcess(line$entityId)
    runs <- improveR::updateProcessRuns(process$id)
    line$runNoAfter <- nrow(runs)
    return(line)
  })
  runNumbers <- dplyr::count(relativeWorkflowAfter,runNoAfter)
  expect_equal(runNumbers[runNumbers$runNoAfter==1,]$n, 1)
  expect_equal(runNumbers[runNumbers$runNoAfter==2,]$n, 4)
  # Without usage
  a <- improveR::updateFileContent(inputFile, "improver.log")
  #TODO without usage needs to be included
  reExecutionPlan <- test4Workflow$createReexecutionPlan()
  reExecutionPlan <- dplyr::filter(reExecutionPlan,is.na(lineage))
  reExecutionPlan$usage<-""
  test4Workflow$executePlan(reExecutionPlan)


  relativeWorkflowAfter <- improveR::byNotEmptyAsDf(executedSteps, function(line) {
    process <- improveR::getMainProcess(line$entityId)
    runs <- improveR::updateProcessRuns(process$id)
    line$runNoAfter <- nrow(runs)
    return(line)
  })
  runNumbers <- dplyr::count(relativeWorkflowAfter,runNoAfter)
  expect_equal(runNumbers[runNumbers$runNoAfter==1,]$n, 1)
  expect_equal(runNumbers[runNumbers$runNoAfter==2,]$n, 3)
  expect_equal(runNumbers[runNumbers$runNoAfter==3,]$n, 1)



  # Test with just local files
  testTree5 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "SimpleWorkflow Reexecute with local")
  localFile <- getWorkflow(testTree)
  localFileTemplate <- localFile$createTemplate()
  localFileTemplate$setWorkflowTreeIdent(testTree5)

  dmName <- names(localFileTemplate$stepTemplates)[grep(pattern = "Step 2", names(localFileTemplate$stepTemplates))]
  dmStep <- localFileTemplate$stepTemplates[[dmName]]


  dmStep$removeStepRemoteFile( "./data.csv")
  dmStep$addStepLocalFile(path = "improver.log", name = "data.csv")

  localFileTemplate$realise()

  # Wait till executed with specific tool
  # Only execute manually
  if (F) {
    testTree5 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "Manual execution, wait for correct tool")
    improveR::setStepTree(handleReport, testTree5)
    improveR::realiseStep(handleReport, run = F)
    # Switch im improveR in browser, run, edit, finish
    # Reexecute in improveRbatch, then it should stop
    report <- improveR::retrieveStep(handleReport)
    improveR::finishRun(handleReport, runserverName = report$runserverName, runserverToolName = report$runserverToolName)
    # Finished
    expect_true(T)

    # FinishCondition in workflow
    # Breakpoint in workflow
    templateWorkflowHandle <- improveR::handlesFromTree(testTree)
    improveR::makeStepsRelative(templateWorkflowHandle)
    templateWorkflowHandle <- improveR::deepWorkflowCopy(templateWorkflowHandle, testTree5)
    templateWorkflow <- improveR::retrieveWorkflow(templateWorkflowHandle)
    templateWorkflow <- improveR::executionOrder(templateWorkflow)

    improveR::setStepBreakpoint(templateWorkflow[3, ]$handle, T)
    improveR::setStepFinishCondition(templateWorkflow[4, ]$handle, runserverName = templateWorkflow[4, ]$runserverName, "improVerse libs")
    improveR::executeWorkflow(templateWorkflowHandle)

    # Execute the not run step 3
    # Execute the other step 4 with improVerse libs, terminate it
    # Report should execute automatically, executeWorflow should finish
    expect_true(T)
  }
})





