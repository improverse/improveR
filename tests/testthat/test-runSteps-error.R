Sys.setenv(TEST_NAME="runSteps-error")

test_that("createTestFolder", {
  improveConnect()
  setEditable(T)
  expect_false(Sys.getenv("IMPROVER_TOKEN")=="")
  TEST_FOLDER <- emptyFolderSetup()
  assign(x = "TEST_FOLDER",value = TEST_FOLDER,envir = globalenv())
})

# Helper function to ensure TEST_FOLDER exists when running tests individually
ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME="runSteps-error")
  if (!exists("TEST_FOLDER", envir = globalenv()) || is.null(get("TEST_FOLDER", envir = globalenv()))) {
    improveR::setEditable(TRUE)
    TEST_FOLDER <- improveR:::emptyFolderSetup()
    assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
    return(TEST_FOLDER)
  }
  return(get("TEST_FOLDER", envir = globalenv()))
}

# Helper function to get FAKE_PATH (must be called after TEST_FOLDER is set)
getFakePath <- function() {
  paste0(get("TEST_FOLDER", envir = globalenv()), "/FAKE")
}

FAKE_RES_ID <- "XXXXXXXXXXXXX"
FAKE_ENTITY_ID <- "wrongrepo:wrongID"
FAKE_LONG_ENTITY_ID <- "http://wrongURL:8843/?path=wrongrepo:wrongID"
FAKE_LABEL <- "FAKE_LABEL"

checkConnected <- function(func) {
  expectedMessage <- "improveConnect was not called or an error was thrown while connecting"
  expectedError <- "not connected"
  tryCatch({
    resource <- func()
  }, error = function(e) {
    expect_equal(as.character(e[1]), expectedError)
  }, finally = function() {
    message <- improveLastLogMessage("ERROR")
    expect_equal(message, expectedMessage)
  })
}

checkConnectedOneArgument <- function(func) {
  expectedMessage <- "improveConnect was not called or an error was thrown while connecting"
  expectedError <- "not connected"
  tryCatch({
    resource <- func(FAKE_LABEL)
  }, error = function(e) {
    expect_equal(as.character(e[1]), expectedError)
  }, finally = function() {
    message <- improveLastLogMessage("ERROR")
    expect_equal(message, expectedMessage)
  })
}

checkConnectedTwoArgument <- function(func) {
  expectedMessage <- "improveConnect was not called or an error was thrown while connecting"
  expectedError <- "not connected"
  tryCatch({
    resource <- func(FAKE_LABEL, FAKE_LABEL)
  }, error = function(e) {
    expect_equal(as.character(e[1]), expectedError)
  }, finally = function() {
    message <- improveLastLogMessage("ERROR")
    expect_equal(message, expectedMessage)
  })
}

test_that("errors in load infrastructure|ics1081", {
  improveDisconnect()
  Sys.setenv(improver.logfile = "improver.log")
  initImproveLogging("INFO")

  checkConnected(loadAllTools)
  checkConnected(loadRunservers)
  checkConnected(loadToolCategories)
  checkConnected(loadMetaDataDefinitions)

  checkConnectedOneArgument(loadRunserver)
  checkConnectedOneArgument(loadGridArguments)
  checkConnectedOneArgument(loadMetaDataDefinition)
  checkConnectedOneArgument(loadMetaDataDefinitionPickList)
  checkConnectedOneArgument(loadProcessesForStep)
  checkConnectedOneArgument(loadProcessesForStepById)
  checkConnectedOneArgument(loadProcessGridArguments)
  checkConnectedOneArgument(loadProcessRuns)
  checkConnectedOneArgument(loadProcessVariables)
  checkConnectedOneArgument(loadToolForRunserver)
  checkConnectedOneArgument(loadToolsForCategory)
  checkConnectedOneArgument(loadToolsForRunserver)

  checkConnectedTwoArgument(loadGridArgumentDefinition)
})

test_that("load with wrong reference|ics1081", {
  Sys.setenv(improver.logfile = "improver.log")
  improveConnect()
  TEST_FOLDER <- ensureTestFolder()
  testFolder <- createFolder(TEST_FOLDER)

  # runserver
  runserver <- loadRunserver(FAKE_LABEL)
  expect_null(runserver)
  message <- improveLastLogMessage("WARNING")
  expect_true(startsWith(message, "No or multiple runservers with this label found:"))

  gridArguments <- loadGridArguments(FAKE_LABEL)
  expect_null(gridArguments)
  message <- improveLastLogMessage("WARN")
  expect_equal(message, "no gridArguments for provider FAKE_LABEL")

  gridArgumentDefinition <- loadGridArgumentDefinition(FAKE_LABEL, FAKE_LABEL)
  expect_null(gridArgumentDefinition)
  message <- improveLastLogMessage("WARN")
  expect_equal(message, "no gridArguments for provider FAKE_LABEL")

  gridArguments <- loadGridArguments("LSF")
  expect_true(is.data.frame(gridArguments))

  gridArgumentDefinition <- loadGridArgumentDefinition("LSF", FAKE_LABEL)
  expect_null(gridArgumentDefinition)
  message <- improveLastLogMessage("WARN")
  expect_equal(message, "FAKE_LABEL not defined for provider LSF")

  processes <- loadProcessesForStep(FAKE_LABEL)
  expect_null(processes)
  message <- improveLastLogMessage("WARN")
  expect_equal(message, "Resource with ID: FAKE_LABEL could not be loaded")

  expectedMessage <- "loadProcessesForStep only possible for type Step. {TEST_FOLDER} is of type Folder"
  processes <- loadProcessesForStep(TEST_FOLDER)
  expect_null(processes)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,as.character(glue::glue(expectedMessage)))

  expectedMessage <- "no process found for the process id: FAKE_LABEL  please note, process can only be loaded via process ID if it has been once loaded via the step."
  processes <- loadProcessesForStepById(FAKE_LABEL)
  message <- improveLastLogMessage("INFO")
  expect_equal(message, expectedMessage)

  processGridArguments <- loadProcessGridArguments(FAKE_LABEL)
  expect_null(processGridArguments)
  message <- improveLastLogMessage("INFO")
  expect_equal(message, expectedMessage)

  processRuns <- loadProcessRuns(FAKE_LABEL)
  expect_null(processRuns)
  message <- improveLastLogMessage("INFO")
  expect_equal(message, expectedMessage)

  processVariables <- loadProcessVariables(FAKE_LABEL)
  expect_null(processVariables)
  message <- improveLastLogMessage("INFO")
  expect_equal(message, expectedMessage)

  expectedMessage <- "no tools found for runserver: FAKE_LABEL"

  tools <- loadToolForRunserver(FAKE_LABEL)
  expect_null(tools)
  message <- improveLastLogMessage("INFO")
  expect_equal(message, expectedMessage)

  tools <- loadToolsForRunserver(FAKE_LABEL)
  expect_null(tools)
  message <- improveLastLogMessage("INFO")
  expect_equal(message, expectedMessage)

  expectedMessage <- "no tools found for category: FAKE_LABEL"
  tools <- loadToolsForCategory(FAKE_LABEL)
  expect_null(tools)
  message <- improveLastLogMessage("INFO")
  expect_equal(message, expectedMessage)

  # wrong tool loading
  nonmem_runserver <- Sys.getenv("NONMEM_RUNSERVER")
  nonmem_tool <- Sys.getenv("NONMEM_TOOL")
  nonmem_tool_instance <- Sys.getenv("NONMEM_TOOL_INSTANCE")

  runserver <- loadRunserver(nonmem_runserver)
  tool <- loadToolForRunserver(runserverId = runserver$id)
  expect_null(tool)
  message <- improveLastLogMessage("ERROR")
  expect_equal(message, "toolName or tool instance name have to be given")

  fakeTool <- paste0(nonmem_tool, "_fake")

  expectedMessage <- "{fakeTool} not unique or does not exist"
  tool <- loadToolForRunserver(runserverId = runserver$id, toolName = fakeTool)
  expect_null(tool)
  message <- improveLastLogMessage("ERROR")
  expect_equal(message,as.character(glue::glue(expectedMessage)))

  tool <- loadToolForRunserver(runserverId = runserver$id, toolInstanceName = nonmem_tool_instance)
  expect_equal(tool$name, nonmem_tool_instance)
  expect_equal(tool$toolName, nonmem_tool)
  expect_equal(tool$label, nonmem_runserver)

  tool <- loadToolForRunserver(runserverId = runserver$id, toolName = nonmem_tool, toolInstanceName = nonmem_tool_instance)
  expect_equal(tool$name, nonmem_tool_instance)
  expect_equal(tool$toolName, nonmem_tool)
  expect_equal(tool$label, nonmem_runserver)

  expectedMessage <- "{toolInstanceName} and {toolName} not unique or does not exist"

  toolName <- FAKE_LABEL
  toolInstanceName <- nonmem_tool_instance
  tool <- loadToolForRunserver(runserverId = runserver$id, toolName = toolName, toolInstanceName = toolInstanceName)
  expect_null(tool)
  message <- improveLastLogMessage("ERROR")
  expect_equal(message,as.character(glue::glue(expectedMessage)))

  toolName <- nonmem_tool
  toolInstanceName <- FAKE_LABEL
  tool <- loadToolForRunserver(runserverId = runserver$id, toolName = toolName, toolInstanceName = toolInstanceName)
  expect_null(tool)
  message <- improveLastLogMessage("ERROR")
  expect_equal(message,as.character(glue::glue(expectedMessage)))
})

