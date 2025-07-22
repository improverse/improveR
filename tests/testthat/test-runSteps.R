Sys.setenv(TEST_NAME="runSteps")

httptest::with_mock_dir("prepare-runSteps",{
  test_that("createTestFolder", {
    clearConnectionData()
    Sys.setenv(IMPROVER_TEST_REPLAY="T")
    improveConnect()
    setEditable(T)
    expect_false(Sys.getenv("IMPROVER_TOKEN")=="")
    TEST_FOLDER <- workflowFilesSetup()
    assign(x = "TEST_FOLDER",value = TEST_FOLDER,envir = globalenv())
  })
})


library(magrittr)

httptest::with_mock_dir("checkRunservers", {
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
})

nonmemBatchStep <- function(testTree) {
  nonmem_runserver <- Sys.getenv("NONMEM_RUNSERVER")
  nonmem_tool <- Sys.getenv("NONMEM_TOOL")
  nonmem_tool_instance <- Sys.getenv("NONMEM_TOOL_INSTANCE")

  stepEnv <- createStepEnv(treeIdent=testTree) %>%
    setStepRunserverName(nonmem_runserver) %>%
    setStepToolName(nonmem_tool) %>%
    setStepCommandLine("<command-file>\r\noutput<process>.txt", append = F) %>%
    setStepRunserverToolName(nonmem_tool_instance)
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

httptest::with_mock_dir("loadChildSteps", {
  test_that("load Child steps|ics1140,ics1205,ics1209,ics1225", {
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
})


test_that("subfolder in step inventory|ics1140,ics1213,ics1214", {
  testTree <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "subfolderInventory")

  stepEnv <- rBatchStep(testTree)
  stepEnv$setStepDescription("Data Manipulation")
  stepEnv$setStepRationale("to manipulate data")
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"), variableName = "command-file")
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.Rmd"), name = "subfolder/test.Rmd", asLink = F)
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/data.csv"))
  stepEnv$realise()
  stepEnv$finishRun()

  step <- stepEnv$getStepEnv()
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
  updateFileStepEnv <- retryFlow$steps[[updateFileStep]]

  updateFileStepEnv$getStepInventory(recurse = T) %>%
    improveR::strip() %>%
    dplyr::filter(inventoryPath == "subfolder/test.Rmd") %>%
    improveR::updateFileContent(localPath = "improver.log")

  #remove methods for workflowtemplate

  #TODO rerun, checkin include pattern for input files
  retryFlow$rerunChangedAndOutdated()




#ask for working copies?
  #ticket, add changed flag to dmg

  retryFlow <- getWorkflow(testTree)



  retryTemplate <- retryFlow$createTemplate()
  retryTemplate$realise()


})

mockStep <- function(tree, dataSet, name, description, dataSet2 = NULL, dataSet3 = NULL) {
  handle <- rBatchStep(tree) %>%
    improveR::setStepDescription(name) %>%
    improveR::setStepRationale(description) %>%
    improveR::addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"), variableName = "command-file") %>%
    improveR::addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.Rmd")) %>%
    improveR::addStepRemoteFile(dataSet, name = "data.csv")




  if (!is.null(dataSet2)) {
    improveR::addStepRemoteFile(handle, dataSet2, name = "data2.csv")
  }
  if (!is.null(dataSet3)) {
    improveR::addStepRemoteFile(handle, dataSet3, name = "data3.csv")
  }
  handle <- handle %>% improveR::realiseStep() %>%
    improveR::finishRun()
?get
  inventory <- improveR::getStepInventory(handle)$data[[1]]
  dataSet <- inventory %>%
    dplyr::filter(name == "chapter15_example_cleaned.rds") %>%
    dplyr::select("entityId") %>% as.character()
  return(dataSet)
}

test_that("DMG spans multiple trees, linear|ics1140", {
  dmgL1 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "DMG L1")
  dmgL2 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "DMG L2")
  dmgL3 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "DMG L3")

  i1 <- mockStep(dmgL1, paste0(TEST_FOLDER, "/data.csv"), "Initial 1", "data comes to system")
  i2 <- mockStep(dmgL1, paste0(TEST_FOLDER, "/data.csv"), "Initial 2", "data comes to system")

  s1t1 <- mockStep(dmgL1, i1, "S1T1", "processing", dataSet2 = i2)

  s2t1 <- mockStep(dmgL1, s1t1, "S2T1", "processing", dataSet2 = i2)

  s1t2 <- mockStep(dmgL2, s1t1, "S1T2", "processing", dataSet2 = s2t1)

  s2t2 <- mockStep(dmgL2, s1t2, "S2T2", "processing", dataSet2 = i2)

  s1t3 <- mockStep(dmgL3, s1t2, "S1T3", "report", dataSet2 = s2t2)



  fullLineageFolder <- createFolder(TEST_FOLDER,"fullLineage")

  lineageWorkflowHandle <- loadChildResources(dmgL3)%>%strip() %>%
    getFullLineage() %>%
      makeStepsRelative() %>%
      detachWorkflowFromResources() %>%
      detachWorkflowFromTrees() %>%
      setWorkflowTreeRootFolder(fullLineageFolder$path) %>%
      executeWorkflow()

  expect_equal(nrow(loadChildResources(fullLineageFolder)$data[[1]]),3)
  expect_equal(nrow(loadChildResources("./DMG L1",fullLineageFolder)$data[[1]]),3)
  expect_equal(nrow(loadChildResources("./DMG L2",fullLineageFolder)$data[[1]]),2)
  expect_equal(nrow(loadChildResources("./DMG L3",fullLineageFolder)$data[[1]]),1)

  lineageWorkflowHandle <- loadChildResources(dmgL3)%>%strip() %>%
    getFullLineage(depth=1) %>%
    makeStepsRelative() %>%
    detachWorkflowFromResources() %>%
    detachWorkflowFromTrees() %>%
    setWorkflowTreeRootFolder(fullLineageFolder$path) %>%
    setWorkflowTreeName("AllInOne") %>%
    executeWorkflow()

  expect_equal(nrow(loadChildResources(fullLineageFolder)$data[[1]]),4)
  expect_equal(nrow(loadChildResources("./AllInOne",fullLineageFolder)$data[[1]]),3)

  fullUsageFolder <- createFolder(TEST_FOLDER,"fullUsage")

  usageWorkflowHandle <- loadFullChildResources(dmgL1)%>%strip() %>%
    dplyr::filter(description=="Initial 2")%>%
    getFullUsage() %>%
    makeStepsRelative() %>%
    detachWorkflowFromResources() %>%
    detachWorkflowFromTrees() %>%
    setWorkflowTreeRootFolder(fullUsageFolder$path) %>%
    executeWorkflow()

  expect_equal(nrow(loadChildResources(fullUsageFolder)$data[[1]]),3)
  expect_equal(nrow(loadChildResources("./DMG L1",fullUsageFolder)$data[[1]]),1)
  expect_equal(nrow(loadChildResources("./DMG L2",fullUsageFolder)$data[[1]]),1)
  expect_equal(nrow(loadChildResources("./DMG L3",fullUsageFolder)$data[[1]]),1)

  usageWorkflowHandle <- loadFullChildResources(dmgL1)%>%strip() %>%
    dplyr::filter(description=="Initial 2")%>%
    getFullUsage(depth=1) %>%
    makeStepsRelative() %>%
    detachWorkflowFromResources() %>%
    detachWorkflowFromTrees() %>%
    setWorkflowTreeRootFolder(fullUsageFolder$path) %>%
    setWorkflowTreeName("AllInOne") %>%
    executeWorkflow()

  expect_equal(nrow(loadChildResources(fullUsageFolder)$data[[1]]),4)
  expect_equal(nrow(loadChildResources("./AllInOne",fullUsageFolder)$data[[1]]),2)


  lineageWorkflowHandle <- loadChildResources(dmgL3)%>%strip() %>%
    getFullLineage() %>%
    makeStepsRelative() %>%
    detachWorkflowFromResources() %>%
    executeWorkflow()

  expect_equal(nrow(loadChildResources("./DMG L1",TEST_FOLDER)$data[[1]]),7)
  expect_equal(nrow(loadChildResources("./DMG L2",TEST_FOLDER)$data[[1]]),4)
  expect_equal(nrow(loadChildResources("./DMG L3",TEST_FOLDER)$data[[1]]),2)


  #import export
  reports <- loadChildResources(dmgL3)%>%strip()
  workflowToexport  <-  reports[1,]%>%
    getFullLineage() %>%
    makeStepsRelative() %>%
    detachWorkflowFromTrees() %>%
    retrieveWorkflow()
  exportWorkflow(workflowToexport,workflowName = "lineageDMG")

  importRepoFolder <- file.path(TEST_FOLDER,"import1")
  createFolder(dirname(importRepoFolder),basename(importRepoFolder))
  importWorkflow("lineageDMG.zip",importRepoFolder)
  #externalLinkMapping


  #DMG L3 why is Initial 1 not included

  #test externalLinks
  #test with subfolders, test with also inputfiles
  #integrate cache
  #zip handling and tempfolderHandling
#mapping of input file variables
  #parental relations in workflows



})

test_that("simple nonmem step with all grid combinations|ics1140,ics1222,ics1213", {
  testTree <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "SimpleNonmem")

  handleNonmem <- nonmemBatchStep(testTree) %>%
    addStepRemoteFile(paste0(TEST_FOLDER, "/STEP1.ctl"), variableName = "command-file") %>%
    addStepRemoteFile(paste0(TEST_FOLDER, "/STEP1.ctl"), name = "folder/file.txt", variableName = "command-file") %>%
    addStepRemoteFile(paste0(TEST_FOLDER, "/example-new.dat"), variableName = "dataset") %>%
    setStepCommandLine("<command-file>\r\noutput<process>.txt", append = F) %>%
    setStepDescription("description1") %>%
    setStepRationale("rational") %>%
    realiseStep() %>%
    finishRun()

  dN <- getCopy(paste0(TEST_FOLDER, "/STEP1.ctl"))

  handleNonmem <- nonmemBatchStep(testTree) %>%
    addStepLocalFile(dN$path, variableName = "command-file") %>%
    addStepRemoteFile(paste0(TEST_FOLDER, "/example-new.dat"), variableName = "dataset") %>%
    setStepDescription("I am showing a nonmem step") %>%
    setStepRationale("so you have seen it") %>%
    realiseStep() %>%
    finishRun()

  inventory <- improveR::getStepInventory(handleNonmem)
  inv <- inventory$data[[1]]
  inv <- inv[inv$name == "output.txt", ]

  invCopy <- improveR::getCopy(inv)

  unlink(dN$path)
  unlink(invCopy$path)

  handle <- improveR::retrieveStep(handleNonmem)
  step <- improveR::getStepResource(handleNonmem)
  step <- improveR::updateResource(step)
  expect_equal(improveR::getStepState(handleNonmem), "FINISHED")
  entityId <- step$entityId

  process <- improveR::loadProcessesForStep(entityId)

  # Create step copy
  stepCopy <- improveR::handleFromStep(step)
  copyHandle <- improveR::retrieveStep(stepCopy)
  testTree2 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "SimpleNonmemCopy")
  stepCopy <- stepCopy %>%
    improveR::setStepTree(testTree2$resourceId) %>%
    improveR:::setStepValue("entityId", NULL) %>%
    improveR::realiseStep() %>%
    improveR::finishRun()

  handle <- improveR::retrieveStep(stepCopy)

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
  checkGrid <- improveR::handlesFromTree(testTree)
  checkGridFlow <- improveR::retrieveWorkflow(checkGrid)

  testTree3 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "SimpleNonmemCopy Grid Check")

  copyGrid <- improveR::deepWorkflowCopy(checkGrid, targetTree = testTree3)
  copyGridFlow <- improveR::retrieveWorkflow(copyGrid)
  improveR::executeWorkflow(copyGrid)

  checkGridCompare <- improveR::handlesFromTree(testTree3)
  checkGridFlowCompare <- improveR::retrieveWorkflow(checkGrid)

  args <- dplyr::filter(checkGridFlow, description == "I am showing a nonmem step")$processes[[1]]$gridArguments[[1]] %>% dplyr::select(argumentName, argumentValue)
  argsCompare <- dplyr::filter(checkGridFlowCompare, description == "I am showing a nonmem step")$processes[[1]]$gridArguments[[1]] %>% dplyr::select(argumentName, argumentValue)
  expect_equal(args, argsCompare)
  expect_equal(nrow(args), 4)
})

test_that("test full workflow|ics1140,ics1211,ics1212,ics1213,ics1214,ics1220", {
  testTree <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "SimpleWorkflow")

  handle <- rBatchStep(testTree) %>%
    improveR::setStepDescription("Data Manipulation") %>%
    improveR::setStepRationale("to manipulate data") %>%
    improveR::addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"), variableName = "command-file") %>%
    improveR::addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.Rmd")) %>%
    improveR::addStepRemoteFile(paste0(TEST_FOLDER, "/data.csv"))

  handle <- improveR::realiseStep(handle)
  handle <- improveR::finishRun(handle)

  handle <- rBatchStep(testTree) %>%
    improveR::setStepDescription("Data Manipulation") %>%
    improveR::setStepRationale("to manipulate data") %>%
    improveR::addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"), variableName = "command-file") %>%
    improveR::addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.Rmd")) %>%
    improveR::addStepRemoteFile(paste0(TEST_FOLDER, "/data.csv"))

  handle <- improveR::realiseStep(handle)

  handle <- improveR::finishRun(handle)
  expect_equal(improveR::getStepState(handle), "FINISHED")

  inventory <- improveR::getStepInventory(handle)$data[[1]]

  cleanDataset <- inventory[inventory$name == "chapter15_example_cleaned.rds", ]

  handleEDA <- rBatchStep(testTree) %>%
    improveR::addStepRemoteFile(paste0(TEST_FOLDER, "/EDA.R"), variableName = "command-file") %>%
    improveR::addStepRemoteFile(cleanDataset) %>%
    improveR::realiseStep()

  handleLM <- rBatchStep(testTree) %>%
    improveR::addStepRemoteFile(paste0(TEST_FOLDER, "/test_lm_plot.R"), variableName = "command-file") %>%
    improveR::addStepRemoteFile(cleanDataset) %>%
    improveR::realiseStep()

  handleEDA <- improveR::finishRun(handleEDA)
  handleLM <- improveR::finishRun(handleLM)

  inventory <- improveR::getStepInventory(handleEDA)$data[[1]]
  byDay <- inventory[inventory$name == "HAMDTL17_by_day.png", ]
  byWeek <- inventory[inventory$name == "HAMDTL17_by_week_therapy.png", ]
  edaTable <- inventory[inventory$name == "EDA_table.html", ]
  inventoryLM <- improveR::getStepInventory(handleLM)$data[[1]]
  modelFit <- inventoryLM[inventoryLM$name == "model_fit.html", ]

  handleReport <- rBatchStep(testTree) %>%
    improveR::addStepRemoteFile(paste0(TEST_FOLDER, "/report.R"), variableName = "command-file") %>%
    improveR::addStepRemoteFile(paste0(TEST_FOLDER, "/report.Rmd")) %>%
    improveR::addStepRemoteFile(byDay) %>%
    improveR::addStepRemoteFile(byWeek) %>%
    improveR::addStepRemoteFile(edaTable) %>%
    improveR::addStepRemoteFile(modelFit) %>%
    improveR::realiseStep() %>%
    improveR::finishRun()

  # Reexecute in new tree
  workflowHandle <- improveR::handlesFromTree(testTree)
  improveR::makeStepsRelative(workflowHandle)
  testTree2 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "SimpleWorkflow Reexecute")
  copyHandle <- improveR::deepWorkflowCopy(workflowHandle, testTree2, createParentalRelation = T)
  improveR::executeWorkflow(copyHandle)

  # Reexecute in same tree
  # With parental
  workflowHandle <- improveR::handlesFromTree(testTree2)
  improveR::makeStepsRelative(workflowHandle)
  copyHandle <- improveR::deepWorkflowCopy(workflowHandle, createParentalRelation = T)
  improveR::executeWorkflow(copyHandle)

  # Without parental
  copyHandle <- improveR::deepWorkflowCopy(workflowHandle, createParentalRelation = F)
  improveR::executeWorkflow(copyHandle)

  # Reexecute in same tree, keep part absolute
  testTree3 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "SimpleWorkflow Reexecute Partial")
  copyHandle <- improveR::deepWorkflowCopy(workflowHandle, testTree3, createParentalRelation = F)
  improveR::executeWorkflow(copyHandle)

  workflowHandle <- improveR::handlesFromTree(testTree3)
  workflow <- improveR::retrieveWorkflow(workflowHandle)
  workflow <- improveR::executionOrder(workflow)

  sortedWorkflow <- improveR::byNotEmptyAsDf(workflow, function(step) {
    res <- improveR::loadResource(step$entityId)
    print(step$entityId)
    print(res$lastModifiedOn)
    step$createdAt <- res$lastModifiedOn
    return(step)
  })

  sortedWorkflow <- sortedWorkflow[order(sortedWorkflow$createdAt), ]

  sortedWorkflow <- sortedWorkflow[4:5, ]

  improveR::makeStepsRelative(sortedWorkflow)

  copyHandle <- improveR::deepWorkflowCopy(workflowHandle, testTree3, createParentalRelation = F)
  improveR::executeWorkflow(copyHandle)

  # Reexecute outdated
  testTree4 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "SimpleWorkflow Reexecute Outdated and Full")
  workflowHandle <- improveR::handlesFromTree(testTree)
  improveR::makeStepsRelative(workflowHandle)
  copyHandle <- improveR::deepWorkflowCopy(workflowHandle, testTree4, createParentalRelation = F)
  # Change data.csv to add as copy
  relativeWorkflow <- improveR::retrieveWorkflow(copyHandle)
  relativeWorkflow <- improveR::executionOrder(relativeWorkflow)
  report <- tail(relativeWorkflow, 1)
  reportLineage <- improveR::lineage(relativeWorkflow, report)
  dataManipulation <- head(reportLineage, 1)
  improveR::changeStepRemoteFile(dataManipulation$handle, "data.csv", asLink = F)

  improveR::executeWorkflow(copyHandle)

  relativeWorkflow <- improveR::retrieveWorkflow(copyHandle)
  relativeWorkflow <- improveR::executionOrder(relativeWorkflow)
  report <- tail(relativeWorkflow, 1)
  reportLineage <- improveR::lineage(relativeWorkflow, report)

  relativeWorkflow <- improveR::byNotEmptyAsDf(relativeWorkflow, function(line) {
    process <- improveR::getMainProcess(line$entityId)
    runs <- improveR::loadProcessRuns(process$id)
    line$runNo <- nrow(runs)
    expect_equal(nrow(runs), 1)
    return(line)
  })

  dataManipulation <- head(reportLineage, 1)
  expect_equal(dataManipulation$description, "Data Manipulation")
  inventory <- improveR::getStepInventory(dataManipulation$handle)$data[[1]]
  inputFile <- inventory[inventory$name == "data.csv", ]
  improveR::updateFileContent(inputFile, "improver.log")
  improveR::rerunChangedAndOutdated(ident = testTree4)

  relativeWorkflowAfter <- improveR::byNotEmptyAsDf(relativeWorkflow, function(line) {
    process <- improveR::getMainProcess(line$entityId)
    runs <- improveR::updateProcessRuns(process$id)
    line$runNoAfter <- nrow(runs)
    return(line)
  })
  runNumbers <- table(relativeWorkflowAfter$runNoAfter)
  expect_equal(runNumbers[["1"]], 1)
  expect_equal(runNumbers[["2"]], 4)
  # Without usage
  a <- improveR::updateFileContent(inputFile, "improver.log")
  improveR::rerunChangedAndOutdated(ident = testTree4, includeUsage = F)

  relativeWorkflowAfter <- improveR::byNotEmptyAsDf(relativeWorkflow, function(line) {
    process <- improveR::getMainProcess(line$entityId)
    runs <- improveR::updateProcessRuns(process$id)
    line$runNoAfter <- nrow(runs)
    return(line)
  })
  runNumbers <- table(relativeWorkflowAfter$runNoAfter)
  expect_equal(runNumbers[["1"]], 1)
  expect_equal(runNumbers[["2"]], 3)
  expect_equal(runNumbers[["3"]], 1)

  # Reexecute all
  improveR::rerunTrees(testTree4)

  relativeWorkflowAfter <- improveR::byNotEmptyAsDf(relativeWorkflow, function(line) {
    process <- improveR::getMainProcess(line$entityId)
    runs <- improveR::updateProcessRuns(process$id)
    line$runNoAfter <- nrow(runs)
    return(line)
  })
  runNumbers <- table(relativeWorkflowAfter$runNoAfter)
  expect_equal(runNumbers[["2"]], 1)
  expect_equal(runNumbers[["3"]], 3)
  expect_equal(runNumbers[["4"]], 1)

  # Test with just local files
  testTree4 <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER, treeName = "SimpleWorkflow Reexecute with local")
  workflowHandle <- improveR::handlesFromTree(testTree)
  improveR::makeStepsRelative(workflowHandle)
  copyHandle <- improveR::deepWorkflowCopy(workflowHandle, testTree4, createParentalRelation = F)

  relativeWorkflow <- improveR::retrieveWorkflow(copyHandle)
  relativeWorkflow <- improveR::executionOrder(relativeWorkflow)
  report <- tail(relativeWorkflow, 1)
  reportLineage <- improveR::lineage(relativeWorkflow, report)
  dataManipulation <- head(reportLineage, 1)

  improveR::removeStepRemoteFile(dataManipulation$handle, "data.csv")
  improveR::addStepLocalFile(dataManipulation$handle, path = "improver.log", name = "data.csv")

  improveR::executeWorkflow(copyHandle)

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





