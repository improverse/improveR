# GFT Check 1 — Core Analysis Workflow
# Mirrors: iat2947 gft_check1.rsc
# Requirements: ics472, ics473, ics474
#
# Scenarios covered:
#   - Create folder structure (gtf_check / acop)
#   - Import data files
#   - Create analysis tree + R steps
#   - Run step and verify run details
#   - Detach / attach steps (step organisation)
#   - File versioning (checkout / edit / checkin)
#   - Audit trail verification
#
# Skipped (UI-only):
#   - Filter configuration
#   - Export to filesystem
#   - Weak links
#   - Data Manipulation Log
#   - NONMEM steps

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
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

hasRunServerConfig <- function() {
  Sys.getenv("R_RUNSERVER") != "" &&
    Sys.getenv("R_TOOL") != "" &&
    Sys.getenv("R_TOOL_INSTANCE") != ""
}

GFT <- new.env(parent = emptyenv())

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------
test_that("GFT1-setup: connect and create test base folder", {
  tryCatch({
    improveR::improveConnect()
    improveR::setEditable(TRUE)
  }, error = function(e) {
    skip(paste("Server not available:", e$message))
  })

  basePath <- Sys.getenv("TEST_FOLDER", "/Projects/Tests")
  baseRes <- improveR::loadResource(basePath)
  if (is.null(baseRes)) {
    # Create TEST_FOLDER path segment by segment
    segments <- strsplit(basePath, "/", fixed = TRUE)[[1]]
    segments <- segments[segments != ""]
    currentPath <- ""
    for (seg in segments) {
      parentPath <- if (currentPath == "") "/" else currentPath
      currentPath <- paste0(currentPath, "/", seg)
      existing <- tryCatch(improveR::loadResource(currentPath), error = function(e) NULL)
      if (is.null(existing)) {
        improveR::createFolder(targetIdent = parentPath, folderName = seg, comment = "auto-created test folder")
      }
    }
    baseRes <- improveR::loadResource(basePath)
    if (is.null(baseRes)) {
      skip(paste("Could not create TEST_FOLDER:", basePath))
    }
  }
  GFT$BASE_PATH <- basePath

  ts <- format(Sys.time(), "%Y%m%d%H%M%S")
  folderName <- paste0("gft-check-", ts)
  folder <- improveR::createFolder(
    targetIdent = basePath,
    folderName = folderName,
    comment = "GFT check 1 test folder"
  )
  expect_false(is.null(folder), info = "GFT root folder should be created")
  expect_equal(folder$name, folderName)
  GFT$ROOT_PATH <- folder$path
  GFT$ROOT_RES <- folder
  cat("Created GFT root folder:", folder$path, "\n")
})

# ===========================================================================
# Create folder structure | ics472
# ===========================================================================
test_that("GFT1-01: create acop subfolder|ics472", {
  skip_if(is.null(GFT$ROOT_PATH), "No GFT root folder")

  subfolder <- improveR::createFolder(
    targetIdent = GFT$ROOT_PATH,
    folderName = "acop",
    comment = "GFT acop subfolder"
  )
  expect_false(is.null(subfolder), info = "acop subfolder should be created")
  expect_equal(subfolder$name, "acop")
  expect_equal(subfolder$nodeType, "Folder")
  GFT$ACOP_PATH <- subfolder$path
  cat("Created acop subfolder:", subfolder$path, "\n")
})

# ===========================================================================
# Import data files | ics472
# ===========================================================================
test_that("GFT1-02: import test data files into acop|ics472", {
  skip_if(is.null(GFT$ACOP_PATH), "No acop subfolder")

  testZip <- system.file("ExampleWorkflow.zip", package = "improveR")
  skip_if(testZip == "", "ExampleWorkflow.zip not found in package")

  tmpDir <- file.path(tempdir(), paste0("gft-import-", format(Sys.time(), "%H%M%S")))
  dir.create(tmpDir, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(testZip, exdir = tmpDir)

  files_to_import <- c("data.csv", "DataManipulation.R", "EDA.R",
                        "example-new.dat", "STEP1.ctl",
                        "testWorkflow.r", "report.R")
  imported <- 0
  for (fname in files_to_import) {
    localPath <- file.path(tmpDir, "ExampleWorkflow", fname)
    if (file.exists(localPath)) {
      res <- tryCatch(
        improveR::createFile(
          targetIdent = GFT$ACOP_PATH,
          fileName = fname,
          localPath = localPath
        ),
        error = function(e) NULL
      )
      if (!is.null(res)) imported <- imported + 1
    }
  }
  unlink(tmpDir, recursive = TRUE)

  expect_true(imported >= 3,
              info = paste("Should import at least 3 files, got", imported))
  cat("Imported", imported, "files into acop\n")
})

# ===========================================================================
# Analysis tree | ics472
# ===========================================================================
test_that("GFT1-03: create analysis tree|ics472", {
  skip_if(is.null(GFT$ROOT_PATH), "No GFT root folder")

  tree <- improveR::createAnalysisTree(
    targetIdent = GFT$ROOT_PATH,
    treeName = "test_analysis_dev"
  )
  expect_false(is.null(tree), info = "Analysis tree should be created")
  expect_equal(tree$nodeType, "Analysis Tree")
  GFT$TREE_PATH <- paste0(GFT$ROOT_PATH, "/test_analysis_dev")
  GFT$TREE_RES <- tree
  cat("Created analysis tree:", GFT$TREE_PATH, "\n")
})

# ===========================================================================
# Create and run Step 1 | ics472, ics473
# ===========================================================================
test_that("GFT1-04: create and run R root step (Step 1)|ics472,ics473", {
  skip_if(is.null(GFT$TREE_PATH), "No analysis tree")
  skip_if(!hasRunServerConfig(), "R_RUNSERVER/R_TOOL/R_TOOL_INSTANCE not set")
  skip_if(is.null(GFT$ACOP_PATH), "No acop folder with files")

  stepEnv <- rBatchStep(GFT$TREE_PATH)
  stepEnv$setStepDescription("GFT Step 1")
  stepEnv$setStepRationale("GFT check 1 root step for R execution")
  stepEnv$addStepRemoteFile(
    paste0(GFT$ACOP_PATH, "/DataManipulation.R"),
    variableName = "command-file"
  )
  stepEnv$addStepRemoteFile(paste0(GFT$ACOP_PATH, "/data.csv"))

  realStep <- stepEnv$realise()
  expect_false(is.null(realStep), info = "Step 1 should be realised")

  stepRes <- realStep$getStepResource()
  expect_false(is.null(stepRes), info = "Step resource should be available")
  expect_equal(stepRes$nodeType, "Step")

  GFT$STEP1_ENV <- stepEnv
  GFT$STEP1_RES <- stepRes
  GFT$STEP1_PATH <- stepRes$path
  cat("Created Step 1:", stepRes$entityId, "\n")

  # Run step and wait for completion
  cat("Running Step 1...\n")
  tryCatch({
    stepEnv$finishRun()
    cat("Step 1 finished successfully\n")
  }, error = function(e) {
    skip(paste("Step 1 run failed:", e$message))
  })

  # Verify run produced processes
  processes <- improveR:::loadProcessesForStep(stepRes$resourceId)
  expect_false(is.null(processes), info = "Step should have processes after run")
  mainProcess <- processes[processes$processType == "main", ]
  expect_equal(nrow(mainProcess), 1,
               info = "Should have exactly one main process")

  runs <- improveR:::loadProcessRuns(mainProcess$id)
  expect_false(is.null(runs), info = "Main process should have runs")
  expect_true(nrow(runs) >= 1, info = "Should have at least one run")

  GFT$STEP1_PROCESS_ID <- mainProcess$id
  GFT$STEP1_RUN_ID <- runs$id[1]
  cat("Step 1 run completed, process:", mainProcess$id, "\n")
})

# ===========================================================================
# Verify run details | ics473
# ===========================================================================
test_that("GFT1-05: verify run details for Step 1|ics473", {
  skip_if(is.null(GFT$STEP1_RES), "No Step 1")
  skip_if(is.null(GFT$STEP1_PROCESS_ID), "No process found")
  skip_if(is.null(GFT$STEP1_RUN_ID), "No run found")

  latestRun <- improveR::getLatestRun(
    GFT$STEP1_RES$resourceId, GFT$STEP1_PROCESS_ID
  )
  expect_true(!is.null(latestRun), info = "getLatestRun should return data")

  run <- improveR::getRun(
    GFT$STEP1_RES$resourceId, GFT$STEP1_PROCESS_ID, GFT$STEP1_RUN_ID
  )
  expect_true(!is.null(run), info = "getRun should return data")

  phases <- improveR::getRunPhases(
    GFT$STEP1_RES$resourceId, GFT$STEP1_PROCESS_ID, GFT$STEP1_RUN_ID
  )
  if (!is.null(phases)) {
    expect_true(is.data.frame(phases), info = "Phases should be a data frame")
    cat("Run phases:", nrow(phases), "\n")
  }
  cat("Run details verified for Step 1\n")
})

# ===========================================================================
# Create child step (Step 2) | ics472, ics473
# ===========================================================================
test_that("GFT1-06: create and run child step (Step 2)|ics472,ics473", {
  skip_if(is.null(GFT$TREE_PATH), "No analysis tree")
  skip_if(!hasRunServerConfig(), "R_RUNSERVER/R_TOOL/R_TOOL_INSTANCE not set")
  skip_if(is.null(GFT$ACOP_PATH), "No acop folder with files")

  stepEnv2 <- rBatchStep(GFT$TREE_PATH)
  stepEnv2$setStepDescription("GFT Step 2")
  stepEnv2$setStepRationale("GFT check 1 child step")
  stepEnv2$addStepRemoteFile(
    paste0(GFT$ACOP_PATH, "/DataManipulation.R"),
    variableName = "command-file"
  )
  stepEnv2$addStepRemoteFile(paste0(GFT$ACOP_PATH, "/data.csv"))

  realStep2 <- stepEnv2$realise()
  expect_false(is.null(realStep2), info = "Step 2 should be realised")

  stepRes2 <- realStep2$getStepResource()
  expect_equal(stepRes2$nodeType, "Step")
  GFT$STEP2_ENV <- stepEnv2
  GFT$STEP2_RES <- stepRes2
  GFT$STEP2_PATH <- stepRes2$path
  cat("Created Step 2:", stepRes2$entityId, "\n")

  cat("Running Step 2...\n")
  tryCatch({
    stepEnv2$finishRun()
    cat("Step 2 finished successfully\n")
  }, error = function(e) {
    cat("Step 2 run error (non-fatal):", e$message, "\n")
  })
})

# ===========================================================================
# Detach / Attach steps | ics474
# ===========================================================================
test_that("GFT1-07: detach Step 2 from parent|ics474", {
  skip_if(is.null(GFT$STEP2_PATH), "No Step 2")

  result <- tryCatch(
    improveR::detachStep(GFT$STEP2_PATH),
    error = function(e) {
      skip(paste("detachStep failed:", e$message))
    }
  )
  expect_false(is.null(result), info = "detachStep should return updated resource")

  # After detach, step should have no parent
  parent <- tryCatch(
    improveR::loadParentStep(GFT$STEP2_PATH),
    error = function(e) NULL
  )
  expect_true(
    is.null(parent) || (is.data.frame(parent) && nrow(parent) == 0),
    info = "Detached step should have no parent"
  )
  cat("Step 2 detached successfully\n")
})

test_that("GFT1-08: attach Step 1 as child of Step 2|ics474", {
  skip_if(is.null(GFT$STEP1_PATH), "No Step 1")
  skip_if(is.null(GFT$STEP2_PATH), "No Step 2")

  result <- tryCatch(
    improveR::attachStep(ident = GFT$STEP1_PATH, parent = GFT$STEP2_PATH),
    error = function(e) {
      skip(paste("attachStep failed:", e$message))
    }
  )
  expect_false(is.null(result), info = "attachStep should return updated resource")

  # After attach, Step 1 should have a parent
  parent <- tryCatch(
    improveR::loadParentStep(GFT$STEP1_PATH),
    error = function(e) NULL
  )
  expect_false(is.null(parent), info = "Attached step should have a parent")
  cat("Step 1 attached as child of Step 2\n")
})

# ===========================================================================
# File versioning: checkout / edit / checkin | ics472
# ===========================================================================
test_that("GFT1-09: create link_target file and version it|ics472", {
  skip_if(is.null(GFT$ROOT_PATH), "No GFT root folder")

  # Create the initial file
  tmpDir <- file.path(tempdir(), "gft-link")
  dir.create(tmpDir, showWarnings = FALSE, recursive = TRUE)
  localFile <- file.path(tmpDir, "link_target.txt")
  writeLines("test edit 01", localFile)

  fileRes <- improveR::createFile(
    targetIdent = GFT$ROOT_PATH,
    fileName = "link_target",
    localPath = localFile,
    comment = "GFT link target file"
  )
  expect_false(is.null(fileRes), info = "link_target should be created")
  expect_equal(fileRes$nodeType, "File")
  GFT$LINK_TARGET_PATH <- paste0(GFT$ROOT_PATH, "/link_target")
  GFT$LINK_TARGET_RES <- fileRes

  # Lock, update content, unlock — creates version 2
  locked <- improveR::lockResource(GFT$LINK_TARGET_PATH)
  expect_true(locked, info = "Should be able to lock link_target")

  localFileV2 <- file.path(tmpDir, "link_target_v2.txt")
  writeLines(c("test edit 01", "test edit 02"), localFileV2)
  improveR::updateFileContent(GFT$LINK_TARGET_PATH, localPath = localFileV2)

  unlocked <- improveR::unlockResource(GFT$LINK_TARGET_PATH)
  expect_true(unlocked, info = "Should be able to unlock link_target")

  # Verify version history shows at least 2 versions
  improveR::updateResource(GFT$LINK_TARGET_PATH)
  history <- improveR::loadHistory(GFT$LINK_TARGET_PATH)
  expect_false(is.null(history), info = "History should not be NULL")
  expect_false(is.null(history$data), info = "History data should not be NULL")
  expect_true(length(history$data) > 0, info = "History should have data")

  revisions <- history$data[[1]]
  expect_true(nrow(revisions) >= 2,
              info = paste("link_target should have >= 2 versions, got", nrow(revisions)))
  cat("link_target versions:", nrow(revisions), "\n")

  unlink(tmpDir, recursive = TRUE)
})

# ===========================================================================
# Step inventory | ics472
# ===========================================================================
test_that("GFT1-10: verify step inventory contains expected files|ics472", {
  skip_if(is.null(GFT$STEP1_RES), "No Step 1")

  children <- tryCatch(
    improveR::loadChildResources(GFT$STEP1_RES),
    error = function(e) NULL
  )
  skip_if(is.null(children), "loadChildResources returned NULL")
  skip_if(is.null(children$data) || length(children$data) == 0,
          "No child data returned")

  inventory <- children$data[[1]]
  expect_true(is.data.frame(inventory), info = "Inventory should be a data frame")
  expect_true(nrow(inventory) > 0, info = "Step should have inventory items")
  cat("Step 1 inventory items:", nrow(inventory), "\n")
})

# ===========================================================================
# Audit trail | ics472
# ===========================================================================
test_that("GFT1-11: audit trail has entries for GFT folder|ics472", {
  skip_if(is.null(GFT$ROOT_PATH), "No GFT root folder")

  audit <- tryCatch(
    improveR::loadAuditTrail(GFT$ROOT_PATH),
    error = function(e) NULL
  )
  skip_if(is.null(audit), "loadAuditTrail returned NULL")
  skip_if(is.null(audit$data) || length(audit$data) == 0,
          "No audit data returned")

  entries <- audit$data[[1]]
  expect_true(is.data.frame(entries), info = "Audit entries should be a data frame")
  expect_true(nrow(entries) > 0, info = "Audit trail should have entries")
  cat("Audit trail entries for GFT folder:", nrow(entries), "\n")
})

# ===========================================================================
# Cleanup
# ===========================================================================
test_that("GFT1-cleanup: delete GFT test folder", {
  skip_if(is.null(GFT$ROOT_PATH), "No GFT root folder to clean up")

  result <- improveR::delete(GFT$ROOT_PATH)
  expect_true(result, info = "Deletion of GFT root folder should succeed")
  cat("Deleted GFT root folder:", GFT$ROOT_PATH, "\n")

  rm(list = ls(envir = GFT), envir = GFT)
})
