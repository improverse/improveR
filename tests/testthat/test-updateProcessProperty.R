# Test updateProcessProperty — verify read-modify-write doesn't erase existing fields

Sys.setenv(TEST_NAME = "updateProcessProperty")

ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME = "updateProcessProperty")
  if (!exists("TEST_FOLDER") || is.null(TEST_FOLDER)) {
    improveR::setEditable(TRUE)
    TEST_FOLDER <- improveR:::workflowFilesSetup()
    assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
    return(TEST_FOLDER)
  }
  return(TEST_FOLDER)
}

test_that("setup updateProcessProperty test", {
  TEST_FOLDER <- ensureTestFolder()
  expect_false(is.null(TEST_FOLDER))

  testTree <- improveR::createAnalysisTree(
    targetIdent = TEST_FOLDER,
    treeName = paste0("UPPTest-", format(Sys.time(), "%H%M%S"))
  )
  assign("UPP_TREE", testTree, envir = globalenv())

  r_runserver <- Sys.getenv("R_RUNSERVER")
  r_tool <- Sys.getenv("R_TOOL")
  r_tool_instance <- Sys.getenv("R_TOOL_INSTANCE")

  stepEnv <- createStepTemplateEnv(treeIdent = testTree)
  stepEnv$setStepRunserverLabel(r_runserver)
  stepEnv$setStepToolLabel(r_tool)
  stepEnv$setStepToolInstance(r_tool_instance)
  stepEnv$setStepDescription("Process Property Test")
  stepEnv$setStepRationale("Testing updateProcessProperty")
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"),
                            variableName = "command-file")
  realised <- stepEnv$realise(run = FALSE)
  stepResource <- realised$getStepResource()
  assign("UPP_STEP", stepResource, envir = globalenv())
  cat("Created step:", stepResource$path, "\n")
})

test_that("updateProcessProperty sets key without erasing existing fields|ics2046", {
  stopifnot("No step created" = exists("UPP_STEP", envir = globalenv()))
  stepResource <- get("UPP_STEP", envir = globalenv())

  # Load the Main process BEFORE update — capture existing fields
  processesBefore <- refreshProcessesForStep(stepResource$resourceId)
  mainBefore <- processesBefore[processesBefore$processType == "main", ]
  expect_equal(nrow(mainBefore), 1)
  cat("Before update — toolId:", mainBefore$toolId,
      "runserverToolId:", mainBefore$runserverToolId, "\n")

  # Store some existing values to verify they survive
  toolIdBefore <- mainBefore$toolId
  runserverToolIdBefore <- mainBefore$runserverToolId

  # Set a new property that didn't exist before
  result <- improveR::updateProcessProperty(
    stepResource$resourceId,
    processName = "Main",
    key = "checkoutIncludePatterns",
    value = "test_script.R\r\ndata.csv"
  )
  expect_false(is.null(result))

  # Verify the new property was set
  processesAfter <- refreshProcessesForStep(stepResource$resourceId)
  mainAfter <- processesAfter[processesAfter$processType == "main", ]
  expect_equal(nrow(mainAfter), 1)

  if ("checkoutIncludePatterns" %in% colnames(mainAfter)) {
    cat("checkoutIncludePatterns set to:", mainAfter$checkoutIncludePatterns, "\n")
    expect_equal(mainAfter$checkoutIncludePatterns, "test_script.R\r\ndata.csv")
  } else {
    cat("Note: checkoutIncludePatterns not visible in process df (may be server-internal)\n")
  }

  # CRITICAL: verify existing fields were NOT erased
  expect_equal(mainAfter$toolId, toolIdBefore,
               info = "toolId must survive updateProcessProperty")
  expect_equal(mainAfter$runserverToolId, runserverToolIdBefore,
               info = "runserverToolId must survive updateProcessProperty")
  cat("After update — toolId:", mainAfter$toolId,
      "runserverToolId:", mainAfter$runserverToolId, "\n")
})

test_that("updateProcessProperty can overwrite an existing key|ics2046", {
  stopifnot("No step created" = exists("UPP_STEP", envir = globalenv()))
  stepResource <- get("UPP_STEP", envir = globalenv())

  # Update the same key with a different value
  result <- improveR::updateProcessProperty(
    stepResource$resourceId,
    processName = "Main",
    key = "checkoutIncludePatterns",
    value = "*.R\r\n*.csv\r\ndata/"
  )
  expect_false(is.null(result))

  processesAfter <- refreshProcessesForStep(stepResource$resourceId)
  mainAfter <- processesAfter[processesAfter$processType == "main", ]
  if ("checkoutIncludePatterns" %in% colnames(mainAfter)) {
    expect_equal(mainAfter$checkoutIncludePatterns, "*.R\r\n*.csv\r\ndata/")
  }
})

test_that("updateProcessProperty returns NULL for non-existent process name", {
  stopifnot("No step created" = exists("UPP_STEP", envir = globalenv()))
  stepResource <- get("UPP_STEP", envir = globalenv())

  result <- improveR::updateProcessProperty(
    stepResource$resourceId,
    processName = "NonExistentProcess",
    key = "foo",
    value = "bar"
  )
  expect_null(result)
})

test_that("cleanup updateProcessProperty test", {
  if (exists("UPP_TREE", envir = globalenv())) {
    tryCatch(improveR::delete(get("UPP_TREE", envir = globalenv())$resourceId),
             error = function(e) NULL)
    rm("UPP_TREE", envir = globalenv())
  }
  if (exists("UPP_STEP", envir = globalenv())) rm("UPP_STEP", envir = globalenv())
  expect_true(TRUE)
})
