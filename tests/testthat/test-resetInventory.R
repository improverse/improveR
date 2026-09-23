# Reset Inventory and filesToKeep tests (ics2047)
# Verifies that runStepResource(resetInventory, filesToKeep) controls
# which outputs survive across reruns.
#
# Test plan:
#   1. Step with R script that produces output_v1.txt → run → verify output_v1
#   2. Update the command file INSIDE the step to produce output_v2.txt →
#      normal rerun → both v1 and v2
#   3. Rerun with resetInventory=TRUE → only v2 (v1 gone)
#   4. Restore both → rerun with resetInventory + filesToKeep(v2) → v2 preserved

Sys.setenv(TEST_NAME = "resetInventory")

ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME = "resetInventory")
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

  stepEnv <- createStepTemplateEnv(treeIdent = testTree)
  stepEnv$setStepRunserverLabel(r_runserver)
  stepEnv$setStepToolLabel(r_tool)
  stepEnv$setStepToolInstance(r_tool_instance)
  return(stepEnv)
}

waitForStep <- function(ident, timeout = 180) {
  elapsed <- 0
  while (elapsed < timeout) {
    step <- improveR::refreshResource(ident)
    if (!is.null(step) && step$runStatus == "FINISHED") return(step)
    Sys.sleep(5)
    elapsed <- elapsed + 5
  }
  stop(paste("Step did not finish within", timeout, "seconds. Status:", step$runStatus))
}

getInventory <- function(stepResource) {
  improveR::unloadFullChildResources(stepResource$resourceId)
  inv <- improveR::loadFullChildResources(stepResource$resourceId)
  if (is.null(inv) || is.null(inv$data[[1]])) return(data.frame())
  inv$data[[1]]
}

# Helper: find a file by name in step inventory and update its content
updateStepFile <- function(stepResource, fileName, localPath) {
  inv <- getInventory(stepResource)
  target <- inv[inv$name == fileName, ]
  if (nrow(target) == 0) stop(paste("File", fileName, "not found in step inventory"))
  resId <- target$resourceId[1]
  improveR::lockResource(resId)
  improveR::updateFileContent(resId, localPath = localPath)
  improveR::unlockResource(resId)
  cat("Updated", fileName, "in step inventory\n")
}

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------
test_that("setup resetInventory test environment", {
  TEST_FOLDER <- ensureTestFolder()
  expect_false(is.null(TEST_FOLDER))

  # Create a dedicated folder for the test with its own command file
  riFolder <- improveR::createFolder(
    targetIdent = TEST_FOLDER,
    folderName = paste0("ri-", uniqueTag()),
    comment = "resetInventory test data"
  )
  assign("RI_FOLDER", riFolder, envir = globalenv())

  # Upload V1 command file: produces output_v1.txt
  scriptV1 <- 'writeLines("v1", "output_v1.txt")'
  writeLines(scriptV1, "ri_cmd.R")
  cmdFile <- improveR::createFile(
    targetIdent = riFolder$resourceId,
    fileName = "ri_cmd.R",
    localPath = "ri_cmd.R",
    comment = "V1 command file"
  )
  unlink("ri_cmd.R")
  expect_false(is.null(cmdFile))
  assign("RI_CMD_FILE", cmdFile, envir = globalenv())

  # Upload a dummy input file
  writeLines("dummy input", "ri_input.csv")
  inputFile <- improveR::createFile(
    targetIdent = riFolder$resourceId,
    fileName = "ri_input.csv",
    localPath = "ri_input.csv",
    comment = "input file"
  )
  unlink("ri_input.csv")
  expect_false(is.null(inputFile))

  # Create analysis tree
  testTree <- improveR::createAnalysisTree(
    targetIdent = TEST_FOLDER,
    treeName = paste0("ResetInvTest-", uniqueTag(6))
  )
  expect_false(is.null(testTree))
  assign("RI_TREE", testTree, envir = globalenv())

  # Create step: command file as COPY (not link), input as link
  stepEnv <- rBatchStep(testTree)
  stepEnv$setStepDescription("ResetInventory Step")
  stepEnv$setStepRationale("Testing resetInventory and filesToKeep")
  stepEnv$addStepRemoteFile(
    cmdFile$entityId,
    variableName = "command-file",
    asLink = FALSE
  )
  stepEnv$addStepRemoteFile(inputFile$entityId, name = "ri_input.csv", asLink = TRUE)

  realised <- stepEnv$realise(run = FALSE)
  stepResource <- realised$getStepResource()
  expect_false(is.null(stepResource))
  assign("RI_STEP", stepResource, envir = globalenv())
  cat("Created step:", stepResource$path, "\n")
})

# ---------------------------------------------------------------------------
# Run 1: script produces output_v1.txt
# ---------------------------------------------------------------------------
test_that("run 1: step produces output_v1|ics2047", {
  stopifnot("No step created" = exists("RI_STEP", envir = globalenv()))
  stepResource <- get("RI_STEP", envir = globalenv())

  improveR::runStepResource(stepResource$resourceId)
  stepResource <- waitForStep(stepResource)
  assign("RI_STEP", stepResource, envir = globalenv())

  inv <- getInventory(stepResource)
  cat("After run 1 inventory:", paste(inv$name, collapse = ", "), "\n")

  # Diagnostic: read command file and stderr to understand what ran
  cmdInv <- inv[inv$name == "DataManipulation.R", ]
  if (nrow(cmdInv) > 0) {
    cmdContent <- tryCatch(getTextString(cmdInv$resourceId), error = function(e) list(data = paste("READ ERROR:", e$message)))
    cat("CMD file content:", cmdContent$data, "\n")
  }
  stderrInv <- inv[inv$name == "_STDERR.txt", ]
  if (nrow(stderrInv) > 0) {
    stderrContent <- tryCatch(getTextString(stderrInv$resourceId), error = function(e) list(data = paste("READ ERROR:", e$message)))
    cat("STDERR:", stderrContent$data, "\n")
  }
  stdoutInv <- inv[inv$name == "_STDOUT.txt", ]
  if (nrow(stdoutInv) > 0) {
    stdoutContent <- tryCatch(getTextString(stdoutInv$resourceId), error = function(e) list(data = paste("READ ERROR:", e$message)))
    cat("STDOUT:", stdoutContent$data, "\n")
  }

  expect_true("output_v1.txt" %in% inv$name, info = "output_v1.txt should exist after run 1")
})

# ---------------------------------------------------------------------------
# Update command file inside step to produce output_v2.txt, normal rerun
# ---------------------------------------------------------------------------
test_that("run 2 (normal): both output_v1 and output_v2|ics2047", {
  stopifnot("No step created" = exists("RI_STEP", envir = globalenv()))
  stepResource <- get("RI_STEP", envir = globalenv())

  # Update the command file IN the step inventory directly
  inv <- getInventory(stepResource)
  stepCmd <- inv[inv$name == "ri_cmd.R", ]
  expect_equal(nrow(stepCmd), 1, info = "ri_cmd.R must be in step inventory")
  scriptV2 <- 'writeLines("v2", "output_v2.txt")'
  writeLines(scriptV2, "ri_cmd_v2.R")
  improveR::updateFileContent(stepCmd$resourceId, localPath = "ri_cmd_v2.R")
  unlink("ri_cmd_v2.R")

  # Normal rerun (no reset)
  improveR::runStepResource(stepResource$resourceId)
  stepResource <- waitForStep(stepResource)
  assign("RI_STEP", stepResource, envir = globalenv())

  inv <- getInventory(stepResource)
  cat("After run 2 (normal) inventory:", paste(inv$name, collapse = ", "), "\n")
  expect_true("output_v1.txt" %in% inv$name, info = "output_v1.txt should still exist (normal rerun)")
  expect_true("output_v2.txt" %in% inv$name, info = "output_v2.txt should exist from new run")
})

# ---------------------------------------------------------------------------
# Run 3: resetInventory → only output_v2 survives
# ---------------------------------------------------------------------------
test_that("run 3 (resetInventory): only output_v2, v1 is gone|ics2047", {
  stopifnot("No step created" = exists("RI_STEP", envir = globalenv()))
  stepResource <- get("RI_STEP", envir = globalenv())

  # Rerun with reset — wipes inventory before run
  improveR::runStepResource(stepResource$resourceId, resetInventory = TRUE)
  stepResource <- waitForStep(stepResource)
  assign("RI_STEP", stepResource, envir = globalenv())

  inv <- getInventory(stepResource)
  cat("After run 3 (resetInventory) inventory:", paste(inv$name, collapse = ", "), "\n")
  expect_false("output_v1.txt" %in% inv$name, info = "output_v1.txt should be gone (inventory was reset)")
  expect_true("output_v2.txt" %in% inv$name, info = "output_v2.txt should exist from new run")
})

# ---------------------------------------------------------------------------
# Run 4: restore both, then resetInventory + filesToKeep(v2)
# ---------------------------------------------------------------------------
test_that("run 4 (resetInventory + filesToKeep): v2 preserved|ics2047", {
  stopifnot("No step created" = exists("RI_STEP", envir = globalenv()))
  stepResource <- get("RI_STEP", envir = globalenv())

  # Update command file IN step inventory to produce BOTH v1 and v2
  inv <- getInventory(stepResource)
  stepCmd <- inv[inv$name == "ri_cmd.R", ]
  scriptBoth <- 'writeLines("v1", "output_v1.txt"); writeLines("v2", "output_v2.txt")'
  writeLines(scriptBoth, "ri_cmd_both.R")
  improveR::updateFileContent(stepCmd$resourceId, localPath = "ri_cmd_both.R")
  unlink("ri_cmd_both.R")

  # Normal run to populate both outputs
  improveR::runStepResource(stepResource$resourceId)
  stepResource <- waitForStep(stepResource)

  inv <- getInventory(stepResource)
  expect_true("output_v1.txt" %in% inv$name, info = "Need both outputs for filesToKeep test")
  expect_true("output_v2.txt" %in% inv$name, info = "Need both outputs for filesToKeep test")
  v2Guid <- inv$resourceId[inv$name == "output_v2.txt"][1]
  cat("output_v2.txt GUID:", v2Guid, "\n")

  # Update command file in step to only produce v2
  inv3 <- getInventory(stepResource)
  stepCmd3 <- inv3[inv3$name == "ri_cmd.R", ]
  scriptV2 <- 'writeLines("v2", "output_v2.txt")'
  writeLines(scriptV2, "ri_cmd_v2b.R")
  improveR::updateFileContent(stepCmd3$resourceId, localPath = "ri_cmd_v2b.R")
  unlink("ri_cmd_v2b.R")

  # Rerun with resetInventory + keep output_v2
  improveR::runStepResource(stepResource$resourceId,
                            resetInventory = TRUE,
                            filesToKeep = v2Guid)
  stepResource <- waitForStep(stepResource)
  assign("RI_STEP", stepResource, envir = globalenv())

  inv <- getInventory(stepResource)
  cat("After run 4 (resetInventory+filesToKeep) inventory:", paste(inv$name, collapse = ", "), "\n")
  expect_true("output_v2.txt" %in% inv$name,
              info = "output_v2.txt should be preserved via filesToKeep")
})

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
test_that("cleanup resetInventory test environment", {
  if (exists("RI_TREE", envir = globalenv())) {
    tryCatch(improveR::delete(get("RI_TREE", envir = globalenv())$resourceId),
             error = function(e) cat("Cleanup warning:", e$message, "\n"))
  }
  if (exists("RI_FOLDER", envir = globalenv())) {
    tryCatch(improveR::delete(get("RI_FOLDER", envir = globalenv())$resourceId),
             error = function(e) NULL)
  }
  for (v in c("RI_CMD_FILE", "RI_TREE", "RI_STEP", "RI_FOLDER")) {
    if (exists(v, envir = globalenv())) rm(list = v, envir = globalenv())
  }
  for (f in Sys.glob("ri_cmd*.R")) unlink(f)
  expect_true(TRUE)
})
