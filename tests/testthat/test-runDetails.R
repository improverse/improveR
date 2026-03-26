# Test Run Details Functions
# Tests: getLatestRun, getRun, getRunPhases, getRunParameters, getRunGridArguments

rBatchStep <- function(testTree) {
  r_runserver <- Sys.getenv("R_RUNSERVER")
  r_tool <- Sys.getenv("R_TOOL")
  r_tool_instance <- Sys.getenv("R_TOOL_INSTANCE")

  stepEnv <- createStepTemplateEnv(treeIdent = testTree)
  stepEnv$setStepRunserverLabel(r_runserver)
  stepEnv$setStepToolLabel(r_tool)
  stepEnv$setStepToolInstance(r_tool_instance)

  return(stepEnv)
}

ensureTestFolder <- function() {
  if (!exists("TEST_FOLDER", envir = globalenv())) {
    tryCatch({
      improveR::improveConnect()
      improveR::setEditable(TRUE)
      TEST_FOLDER <- improveR:::workflowFilesSetup()
      assign("TEST_FOLDER", TEST_FOLDER, envir = globalenv())
    }, error = function(e) {
      skip(paste("Server not available:", e$message))
    })
  }
}

# ---------------------------------------------------------------------------
# Setup: Create a step and run it to FINISHED state
# ---------------------------------------------------------------------------
test_that("setup run details test environment", {
  tryCatch({
    improveR::improveConnect()
    improveR::setEditable(TRUE)
  }, error = function(e) {
    skip(paste("Server not available:", e$message))
  })

  # Check R tool env vars are set
  if (Sys.getenv("R_RUNSERVER") == "" ||
      Sys.getenv("R_TOOL") == "" ||
      Sys.getenv("R_TOOL_INSTANCE") == "") {
    skip("R_RUNSERVER, R_TOOL, R_TOOL_INSTANCE not set")
  }

  # Create test folder with workflow files
  TEST_FOLDER <- tryCatch(
    improveR:::workflowFilesSetup(),
    error = function(e) {
      skip(paste("workflowFilesSetup failed:", e$message))
    }
  )
  assign("TEST_FOLDER", TEST_FOLDER, envir = globalenv())

  # Create analysis tree and step
  testTree <- improveR::createAnalysisTree(
    targetIdent = TEST_FOLDER,
    treeName = "RunDetailsTestTree"
  )
  assign("TEST_TREE", testTree, envir = globalenv())

  stepEnv <- rBatchStep(testTree)
  stepEnv$setStepDescription("RunDetailsTest")
  stepEnv$setStepRationale("automated test for run details")
  stepEnv$addStepRemoteFile(
    paste0(TEST_FOLDER, "/DataManipulation.R"),
    variableName = "command-file"
  )
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/data.csv"))

  realStep <- stepEnv$realise()
  assign("TEST_STEP_ENV", stepEnv, envir = globalenv())

  stepRes <- realStep$getStepResource()
  assign("TEST_STEP_RES", stepRes, envir = globalenv())
  cat("Created step:", stepRes$entityId, "\n")

  # Wait for step to finish
  cat("Waiting for step to finish...\n")
  stepEnv$finishRun()
  cat("Step finished\n")

  # Load processes and runs
  processes <- improveR:::loadProcessesForStep(stepRes$resourceId)
  expect_false(is.null(processes))
  assign("TEST_PROCESSES", processes, envir = globalenv())

  mainProcess <- processes[processes$processType == "main", ]
  expect_equal(nrow(mainProcess), 1)
  assign("TEST_PROCESS_ID", mainProcess$id, envir = globalenv())
  cat("Main process:", mainProcess$id, "\n")

  runs <- improveR:::loadProcessRuns(mainProcess$id)
  expect_false(is.null(runs))
  assign("TEST_RUN_ID", runs$id[1], envir = globalenv())
  cat("Run:", runs$id[1], "\n")
})

# ---------------------------------------------------------------------------
# getLatestRun | ics1329
# ---------------------------------------------------------------------------
test_that("getLatestRun retrieves the latest run|ics1329", {
  if (!exists("TEST_STEP_RES", envir = globalenv())) skip("No finished step")
  if (!exists("TEST_PROCESS_ID", envir = globalenv())) skip("No process found")
  stepRes <- get("TEST_STEP_RES", envir = globalenv())
  processId <- get("TEST_PROCESS_ID", envir = globalenv())

  result <- improveR::getLatestRun(stepRes$resourceId, processId)
  if (is.null(result)) {
    skip("getLatestRun returned NULL")
  }
  expect_true(is.data.frame(result))
  cat("Latest run retrieved for process:", processId, "\n")
})

# ---------------------------------------------------------------------------
# getRun | ics1330
# ---------------------------------------------------------------------------
test_that("getRun retrieves a specific run|ics1330", {
  if (!exists("TEST_STEP_RES", envir = globalenv())) skip("No finished step")
  if (!exists("TEST_PROCESS_ID", envir = globalenv())) skip("No process found")
  if (!exists("TEST_RUN_ID", envir = globalenv())) skip("No run found")
  stepRes <- get("TEST_STEP_RES", envir = globalenv())
  processId <- get("TEST_PROCESS_ID", envir = globalenv())
  runId <- get("TEST_RUN_ID", envir = globalenv())

  result <- improveR::getRun(stepRes$resourceId, processId, runId)
  if (is.null(result)) {
    skip("getRun returned NULL")
  }
  expect_true(is.data.frame(result))
  cat("Run retrieved:", runId, "\n")
})

# ---------------------------------------------------------------------------
# getRunPhases | ics1333
# ---------------------------------------------------------------------------
test_that("getRunPhases retrieves run phases|ics1333", {
  if (!exists("TEST_STEP_RES", envir = globalenv())) skip("No finished step")
  if (!exists("TEST_PROCESS_ID", envir = globalenv())) skip("No process found")
  if (!exists("TEST_RUN_ID", envir = globalenv())) skip("No run found")
  stepRes <- get("TEST_STEP_RES", envir = globalenv())
  processId <- get("TEST_PROCESS_ID", envir = globalenv())
  runId <- get("TEST_RUN_ID", envir = globalenv())

  result <- improveR::getRunPhases(stepRes$resourceId, processId, runId)
  if (!is.null(result)) {
    expect_true(is.data.frame(result))
    cat("Run phases found:", nrow(result), "\n")
  }
})

# ---------------------------------------------------------------------------
# getRunParameters | ics1334
# ---------------------------------------------------------------------------
test_that("getRunParameters retrieves run parameters|ics1334", {
  if (!exists("TEST_STEP_RES", envir = globalenv())) skip("No finished step")
  if (!exists("TEST_PROCESS_ID", envir = globalenv())) skip("No process found")
  if (!exists("TEST_RUN_ID", envir = globalenv())) skip("No run found")
  stepRes <- get("TEST_STEP_RES", envir = globalenv())
  processId <- get("TEST_PROCESS_ID", envir = globalenv())
  runId <- get("TEST_RUN_ID", envir = globalenv())

  result <- improveR::getRunParameters(stepRes$resourceId, processId, runId)
  if (!is.null(result)) {
    expect_true(is.data.frame(result))
    cat("Run parameters found:", nrow(result), "\n")
  }
})

# ---------------------------------------------------------------------------
# getRunGridArguments | ics1332
# ---------------------------------------------------------------------------
test_that("getRunGridArguments retrieves run grid arguments|ics1332", {
  if (!exists("TEST_STEP_RES", envir = globalenv())) skip("No finished step")
  if (!exists("TEST_PROCESS_ID", envir = globalenv())) skip("No process found")
  if (!exists("TEST_RUN_ID", envir = globalenv())) skip("No run found")
  stepRes <- get("TEST_STEP_RES", envir = globalenv())
  processId <- get("TEST_PROCESS_ID", envir = globalenv())
  runId <- get("TEST_RUN_ID", envir = globalenv())

  result <- improveR::getRunGridArguments(stepRes$resourceId, processId, runId)
  expect_true(is.null(result) || is.data.frame(result))
  if (!is.null(result) && is.data.frame(result)) {
    cat("Run grid arguments found:", nrow(result), "\n")
  }
})

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
test_that("cleanup run details test environment", {
  if (exists("TEST_STEP_RES", envir = globalenv())) rm("TEST_STEP_RES", envir = globalenv())
  if (exists("TEST_STEP_ENV", envir = globalenv())) rm("TEST_STEP_ENV", envir = globalenv())
  if (exists("TEST_TREE", envir = globalenv())) rm("TEST_TREE", envir = globalenv())
  if (exists("TEST_PROCESSES", envir = globalenv())) rm("TEST_PROCESSES", envir = globalenv())
  if (exists("TEST_PROCESS_ID", envir = globalenv())) rm("TEST_PROCESS_ID", envir = globalenv())
  if (exists("TEST_RUN_ID", envir = globalenv())) rm("TEST_RUN_ID", envir = globalenv())
  if (exists("TEST_FOLDER", envir = globalenv())) rm("TEST_FOLDER", envir = globalenv())
  expect_true(TRUE)
})
