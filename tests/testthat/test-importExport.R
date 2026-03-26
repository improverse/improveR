# Test suite for import/export workflow functionality
# Tests the complete round-trip of exporting and importing workflows
# including dependencies preservation, file handling, and mapping configurations

Sys.setenv(TEST_NAME="importExport")

# Import/export tests - now running on all repository versions
# Previously skipped for versions < 4.4, but now enabled for testing

# Helper: delete a child resource if it exists (clean slate for imports)
cleanImportTarget <- function(parentFolder, childName) {
  existing <- tryCatch(loadResource(paste0("./", childName), parentFolder), error = function(e) NULL)
  if (!is.null(existing)) {
    tryCatch(delete(existing), error = function(e) {
      cat("Could not delete existing", childName, ":", e$message, "\n")
    })
  }
}

# Helper function to ensure TEST_FOLDER exists when running tests individually
ensureTestFolder <- function() {

  Sys.setenv(TEST_NAME="importExport")
  if (!exists("TEST_FOLDER") || is.null(TEST_FOLDER)) {
    setEditable(TRUE)
    TEST_FOLDER <- workflowFilesSetup()
    assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
    return(TEST_FOLDER)
  }
  return(TEST_FOLDER)
}

# Helper to create a simple R batch step
createTestStep <- function(tree, name, description, commandFile, dataFile = NULL) {
  r_runserver <- Sys.getenv("R_RUNSERVER")
  r_tool <- Sys.getenv("R_TOOL")
  r_tool_instance <- Sys.getenv("R_TOOL_INSTANCE")

  stepEnv <- createStepTemplateEnv(treeIdent = tree)
  stepEnv$setStepRunserverLabel(r_runserver)
  stepEnv$setStepToolLabel(r_tool)
  stepEnv$setStepToolInstance(r_tool_instance)
  stepEnv$setStepDescription(description)
  stepEnv$setStepRationale(paste("Rationale for", name))
  stepEnv$addStepRemoteFile(commandFile, variableName = "command-file")

  if (!is.null(dataFile)) {
    stepEnv$addStepRemoteFile(dataFile, name = "data.csv")
  }

  return(stepEnv)
}

# Helper to verify workflow structure after import
verifyWorkflowStructure <- function(originalWorkflow, importedTree) {
  importedWorkflow <- getWorkflow(importedTree)

  # Check step count
  originalSteps <- originalWorkflow$df()
  importedSteps <- importedWorkflow$df()

  # Diagnostic output in verifyWorkflowStructure
  cat("\nIn verifyWorkflowStructure:\n")
  cat("originalSteps nrow:", nrow(originalSteps), "\n")
  cat("importedSteps nrow:", nrow(importedSteps), "\n")
  if (nrow(importedSteps) > 0) {
    cat("importedSteps columns:", paste(names(importedSteps), collapse=", "), "\n")
    cat("importedSteps$fullName:", importedSteps$fullName, "\n")
  }

  # Step count should match
  expect_equal(nrow(originalSteps), nrow(importedSteps))

  # Check step descriptions preserved
  expect_setequal(originalSteps$description, importedSteps$description)

  # Check dependencies preserved (by step names)
  originalDeps <- originalWorkflow$internalLinks
  importedDeps <- importedWorkflow$internalLinks

  if (!is.null(originalDeps)) {
    expect_equal(nrow(originalDeps), nrow(importedDeps),
                 info = "Internal link count should match")
  }

  return(importedWorkflow)
}

test_that("Setup test environment", {
  Sys.setenv(IMPROVER_TEST_REPLAY="T")

  # Try to connect if not already connected
  if (!improveConnected()) {
    tryCatch({
      improveConnect()
    }, error = function(e) {
      # Connection might fail but we may have a token anyway
    })
  }

  setEditable(TRUE)
  TEST_FOLDER <- workflowFilesSetup()
  expect_false(Sys.getenv("IMPROVER_TOKEN") == "")
  assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
})

test_that("Basic export and import round trip", {
  # Ensure TEST_FOLDER exists for individual test runs
  TEST_FOLDER <- ensureTestFolder()

  # Create source tree with simple workflow
  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                  treeName = "ExportSource")

  # Create two independent steps
  step1 <- createTestStep(sourceTree, "Step1", "First step",
                         paste0(TEST_FOLDER, "/DataManipulation.R"),
                         paste0(TEST_FOLDER, "/data.csv"))
  step1Real <- step1$realise()
  step1$finishRun()


  step2 <- createTestStep(sourceTree, "Step2", "Second step",
                         paste0(TEST_FOLDER, "/EDA.R"),
                         paste0(TEST_FOLDER, "/data.csv"))
  step2Real <- step2$realise()
  step2$finishRun()

  # Get workflow and export
  workflow <- getWorkflow(sourceTree)
  exportWorkflow(workflow, workflowName = "TestBasicExport",
                targetFolder = tempdir())

  exportFile <- file.path(tempdir(), "TestBasicExport.zip")
  expect_true(file.exists(exportFile),
              info = "Export zip file should be created")

  # Verify mapping files created
  expect_true(file.exists(file.path(tempdir(), "TestBasicExportLinkMapping.json")),
              info = "Link mapping file should be created")
  expect_true(file.exists(file.path(tempdir(), "TestBasicExportToolMapping.json")),
              info = "Tool mapping file should be created")

  # Delete source tree to ensure import doesn't rely on it
  delete(sourceTree)

  # Import to new location
  cleanImportTarget(TEST_FOLDER, "ImportTarget1")
  importFolder <- createFolder(TEST_FOLDER, "ImportTarget1")
  importWorkflow(exportFile, importFolder)

  importTree <- loadResource("./ExportSource", importFolder)

  # Diagnostic output for Basic export test
  cat("\n=== Diagnostic output for Basic export test ===\n")
  cat("importTree class:", class(importTree), "\n")

  # Verify structure preserved
  importedWorkflow <- verifyWorkflowStructure(workflow, importTree)

  # Clean up
  unlink(exportFile)
  unlink(file.path(tempdir(), "TestBasicExportLinkMapping.json"))
  unlink(file.path(tempdir(), "TestBasicExportToolMapping.json"))
})

test_that("Export and import with linear dependencies", {
  # Ensure TEST_FOLDER exists for individual test runs
  TEST_FOLDER <- ensureTestFolder()

  # Create workflow with linear dependencies: Step1 → Step2 → Step3
  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                  treeName = "LinearDependencies")

  # Step 1: Data preparation
  step1 <- createTestStep(sourceTree, "Step1", "Data prep",
                         paste0(TEST_FOLDER, "/DataManipulation.R"),
                         paste0(TEST_FOLDER, "/data.csv"))
  step1Real <- step1$realise()
  step1$finishRun()

  # Get output from step1
  inventory1 <- step1Real$getStepInventory()$data[[1]]
  outputFile1 <- inventory1[inventory1$name == "chapter15_example_cleaned.rds", ]

  # Step 2: Analysis using step1 output
  step2 <- createTestStep(sourceTree, "Step2", "Analysis",
                         paste0(TEST_FOLDER, "/EDA.R"))
  step2$addStepRemoteFile(outputFile1, name = "chapter15_example_cleaned.rds")
  step2Real <- step2$realise()
  step2$finishRun()

  # Get output from step2
  inventory2 <- step2Real$getStepInventory()$data[[1]]
  outputFile2 <- inventory2[inventory2$name == "EDA_table.html", ]

  # Step 3: Report using step2 output
  step3 <- createTestStep(sourceTree, "Step3", "Report",
                         paste0(TEST_FOLDER, "/report.R"))
  step3$addStepRemoteFile(outputFile2, name = "EDA_table.html")
  step3Real <- step3$realise()
  step3$finishRun()

  # Export workflow
  workflow <- getWorkflow(sourceTree)
  exportWorkflow(workflow, workflowName = "LinearDeps",
                targetFolder = tempdir())

  exportFile <- file.path(tempdir(), "LinearDeps.zip")

  # Delete source to test true import
  delete(sourceTree)

  # Import
  cleanImportTarget(TEST_FOLDER, "ImportLinear")
  importFolder <- createFolder(TEST_FOLDER, "ImportLinear")
  importWorkflow(exportFile, importFolder)

  importTree <- loadResource("./LinearDependencies",importFolder)
  # Verify dependencies preserved
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()

  # Check that Step3 depends on Step2
  step3Import <- importedSteps[importedSteps$description == "Report", ]

  # Diagnostic output
  cat("\n=== Diagnostic output for Linear Dependencies test ===\n")
  cat("importedSteps:\n")
  print(importedSteps)
  cat("\nstep3Import:\n")
  print(step3Import)
  cat("\nstep3Import$fullName:", step3Import$fullName, "\n")
  cat("class(step3Import$fullName):", class(step3Import$fullName), "\n")
  cat("length(step3Import$fullName):", length(step3Import$fullName), "\n")
  cat("\nimportedWorkflow$steps class:", class(importedWorkflow$steps), "\n")
  cat("ls(importedWorkflow$steps):\n")
  print(ls(importedWorkflow$steps))

  # Check if fullName exists and is valid
  if (is.null(step3Import$fullName) || length(step3Import$fullName) == 0 || any(is.na(step3Import$fullName))) {
    cat("ERROR: step3Import$fullName is NULL, NA, or empty!\n")
    cat("Available columns in step3Import:\n")
    print(names(step3Import))
  }

  step3ImportEnv <- importedWorkflow$steps[[step3Import$fullName]]
  expect_true(
    is.environment(step3ImportEnv$dependencies),
    info = "Step3 should have dependencies"
  )

  # Clean up
  unlink(exportFile)
  unlink(file.path(tempdir(), "LinearDepsLinkMapping.json"))
  unlink(file.path(tempdir(), "LinearDepsToolMapping.json"))
})

test_that("Export and import with branching dependencies", {
  # Ensure TEST_FOLDER exists for individual test runs
  TEST_FOLDER <- ensureTestFolder()

  # Create workflow: Step1 → Step2
  #                      ↘ Step3
  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                  treeName = "BranchingDeps")

  # Step 1: Common data source
  step1 <- createTestStep(sourceTree, "Step1", "Data source",
                         paste0(TEST_FOLDER, "/DataManipulation.R"),
                         paste0(TEST_FOLDER, "/data.csv"))
  step1Real <- step1$realise()
  step1$finishRun()

  inventory1 <- step1Real$getStepInventory()$data[[1]]
  outputFile1 <- inventory1[inventory1$name == "chapter15_example_cleaned.rds", ]

  # Step 2: First branch
  step2 <- createTestStep(sourceTree, "Step2", "Branch A",
                         paste0(TEST_FOLDER, "/EDA.R"))
  step2$addStepRemoteFile(outputFile1, name = "chapter15_example_cleaned.rds")
  step2Real <- step2$realise()
  step2$finishRun()

  # Step 3: Second branch
  step3 <- createTestStep(sourceTree, "Step3", "Branch B",
                         paste0(TEST_FOLDER, "/test_lm_plot.R"))
  step3$addStepRemoteFile(outputFile1, name = "chapter15_example_cleaned.rds")
  step3Real <- step3$realise()
  step3$finishRun()

  # Export and import
  workflow <- getWorkflow(sourceTree)
  exportWorkflow(workflow, workflowName = "BranchingDeps",
                targetFolder = tempdir())

  exportFile <- file.path(tempdir(), "BranchingDeps.zip")
  delete(sourceTree)

  cleanImportTarget(TEST_FOLDER, "ImportBranching")
  importFolder <- createFolder(TEST_FOLDER, "ImportBranching")
  importWorkflow(exportFile, importFolder)

  importTree <- loadResource("./BranchingDeps", importFolder)
  # Verify both branches have correct dependencies
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()

  branchA <- importedSteps[importedSteps$description == "Branch A", ]
  branchB <- importedSteps[importedSteps$description == "Branch B", ]

  # Diagnostic output for branching test
  cat("\n=== Diagnostic output for Branching Dependencies test ===\n")
  cat("branchA$fullName:", branchA$fullName, "\n")
  cat("branchB$fullName:", branchB$fullName, "\n")
  cat("ls(importedWorkflow$steps):", ls(importedWorkflow$steps), "\n")

  # Check internalLinks
  cat("importedWorkflow$internalLinks:\n")
  if (!is.null(importedWorkflow$internalLinks)) {
    print(importedWorkflow$internalLinks[, c("sourceStep", "targetStep", "name")])
  } else {
    cat("  NULL\n")
  }

  # Check remoteFiles of each step
  for (sn in ls(importedWorkflow$steps)) {
    stepEnv <- importedWorkflow$steps[[sn]]
    rf <- stepEnv$stepDf$remoteFiles[[1]]
    cat("Step", sn, "remoteFiles:\n")
    if (!is.null(rf) && nrow(rf) > 0) {
      cat("  cols:", paste(names(rf), collapse=", "), "\n")
      cat("  nrow:", nrow(rf), "\n")
      for (r in seq_len(nrow(rf))) {
        cat("  [", r, "] name=", rf$name[r], " asLink=", rf$asLink[r], " ident=", rf$ident[r], "\n")
      }
    } else {
      cat("  empty or NULL\n")
    }
  }

  branchAEnv <- importedWorkflow$steps[[branchA$fullName]]
  branchBEnv <- importedWorkflow$steps[[branchB$fullName]]

  # Check dependencies - should have items besides 'load' function
  branchADependenciesItems <- setdiff(ls(branchAEnv$dependencies), "load")
  branchBDependenciesItems <- setdiff(ls(branchBEnv$dependencies), "load")
  cat("branchA dependencies:", paste(branchADependenciesItems, collapse=", "), "\n")
  cat("branchB dependencies:", paste(branchBDependenciesItems, collapse=", "), "\n")

  expect_true(
    length(branchADependenciesItems) > 0,
    info = "Branch A should have dependencies"
  )
  expect_true(
    length(branchBDependenciesItems) > 0,
    info = "Branch B should have dependencies"
  )

  # Clean up
  unlink(exportFile)
  unlink(file.path(tempdir(), "BranchingDepsLinkMapping.json"))
  unlink(file.path(tempdir(), "BranchingDepsToolMapping.json"))
})

test_that("Tool mapping validation and application", {
  # Ensure TEST_FOLDER exists for individual test runs
  TEST_FOLDER <- ensureTestFolder()

  # Create workflow with specific tool configuration
  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                  treeName = "ToolMappingTest")

  step1 <- createTestStep(sourceTree, "Step1", "Tool test",
                         paste0(TEST_FOLDER, "/DataManipulation.R"),
                         paste0(TEST_FOLDER, "/data.csv"))
  step1Real <- step1$realise()
  step1$finishRun()

  workflow <- getWorkflow(sourceTree)
  exportWorkflow(workflow, workflowName = "ToolTest",
                targetFolder = tempdir())

  # Read and modify tool mapping
  toolMappingPath <- file.path(tempdir(), "ToolTestToolMapping.json")
  toolMapping <- jsonlite::read_json(toolMappingPath, simplifyVector = TRUE)

  # Test with valid mapping (using same values for test)
  toolMapping$runserverLabel <- Sys.getenv("R_RUNSERVER")
  toolMapping$toolLabel <- Sys.getenv("R_TOOL")
  toolMapping$toolInstance <- Sys.getenv("R_TOOL_INSTANCE")

  jsonlite::write_json(toolMapping, toolMappingPath, pretty = TRUE)

  # Import should succeed with valid mapping
  cleanImportTarget(TEST_FOLDER, "ImportToolValid")
  importFolder <- createFolder(TEST_FOLDER, "ImportToolValid")
  # Allow messages during import, just no warnings or errors
  suppressMessages(
    expect_warning(
      importWorkflow(file.path(tempdir(), "ToolTest.zip"), importFolder),
      NA
    )
  )

  # Test with invalid tool mapping
  toolMapping$runserverLabel <- "nonexistent-server"
  jsonlite::write_json(toolMapping, toolMappingPath, pretty = TRUE)

  cleanImportTarget(TEST_FOLDER, "ImportToolInvalid")
  importFolder2 <- createFolder(TEST_FOLDER, "ImportToolInvalid")
  expect_error(
    importWorkflow(file.path(tempdir(), "ToolTest.zip"), importFolder2),
    regexp = "Import aborted.*Invalid tool mappings",
    info = "Should error on invalid runserver"
  )

  # Clean up
  delete(sourceTree)
  unlink(file.path(tempdir(), "ToolTest.zip"))
  unlink(toolMappingPath)
  unlink(file.path(tempdir(), "ToolTestLinkMapping.json"))
})

test_that("Link mapping with existing resources", {
  # Ensure TEST_FOLDER exists for individual test runs
  TEST_FOLDER <- ensureTestFolder()

  # Extract test data to get local files
  testZip <- system.file("ExampleWorkflow.zip", package = "improveR")
  tempTestDir <- tempfile("testdata")
  dir.create(tempTestDir)
  utils::unzip(testZip, exdir = tempTestDir)

  # Create some resources to map to
  existingFile <- createFile(TEST_FOLDER,
                            fileName = "existing_data.csv",
                            localPath = file.path(tempTestDir, "ExampleWorkflow", "data.csv"))

  # Check if file was created successfully
  expect_true(!is.null(existingFile),
              info = "existingFile should be created")
  expect_true(!is.null(existingFile$entityId),
              info = "existingFile should have entityId")

  # Create workflow with external links
  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                  treeName = "LinkMappingTest")

  step1 <- createTestStep(sourceTree, "Step1", "Link test",
                         paste0(TEST_FOLDER, "/DataManipulation.R"),
                         paste0(TEST_FOLDER, "/data.csv"))
  step1Real <- step1$realise()
  step1$finishRun()

  workflow <- getWorkflow(sourceTree)
  exportWorkflow(workflow, workflowName = "LinkTest",
                targetFolder = tempdir())

  # Modify link mapping to use existing resource
  linkMappingPath <- file.path(tempdir(), "LinkTestLinkMapping.json")
  linkMapping <- jsonlite::read_json(linkMappingPath, simplifyVector = TRUE)

  if (nrow(linkMapping) > 0) {
    # Map first link to existing file
    linkMapping$ident[1] <- existingFile$entityId
    jsonlite::write_json(linkMapping, linkMappingPath, pretty = TRUE)
  }

  # Import with mapping — may emit a hash mismatch warning if the mapped
  # resource has different content than the export's recorded filehash.
  # This is informational and does not block the import.
  cleanImportTarget(TEST_FOLDER, "ImportLinkMapped")
  importFolder <- createFolder(TEST_FOLDER, "ImportLinkMapped")
  suppressWarnings(
    importWorkflow(file.path(tempdir(), "LinkTest.zip"), importFolder)
  )

  # Test with invalid link mapping
  linkMapping$ident[1] <- "invalid-entity-id"
  jsonlite::write_json(linkMapping, linkMappingPath, pretty = TRUE)

  cleanImportTarget(TEST_FOLDER, "ImportLinkInvalid")
  importFolder2 <- createFolder(TEST_FOLDER, "ImportLinkInvalid")
  expect_error(
    importWorkflow(file.path(tempdir(), "LinkTest.zip"), importFolder2),
    regexp = "Import aborted.*Invalid link mappings",
    info = "Should error on invalid resource mapping"
  )

  # Clean up
  delete(sourceTree)
  unlink(file.path(tempdir(), "LinkTest.zip"))
  unlink(linkMappingPath)
  unlink(file.path(tempdir(), "LinkTestToolMapping.json"))
  unlink(tempTestDir, recursive = TRUE)
})

test_that("Export and import with subfolders", {
  # Ensure TEST_FOLDER exists for individual test runs
  TEST_FOLDER <- ensureTestFolder()

  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                  treeName = "SubfolderTest")

  # Create step with files in subfolders
  step1 <- createTestStep(sourceTree, "Step1", "Subfolder test",
                         paste0(TEST_FOLDER, "/DataManipulation.R"))
  # Add file to subfolder - paths must start with ./
  step1$addStepRemoteFile(paste0(TEST_FOLDER, "/data.csv"),
                         name = "./input/data.csv", asLink = FALSE)
  step1$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.Rmd"),
                         name = "./docs/readme.Rmd", asLink = FALSE)
  step1Real <- step1$realise()
  step1$finishRun()

  # Export
  workflow <- getWorkflow(sourceTree)
  exportWorkflow(workflow, workflowName = "SubfolderExport",
                targetFolder = tempdir())

  exportFile <- file.path(tempdir(), "SubfolderExport.zip")

  # Delete source
  delete(sourceTree)

  # Import
  cleanImportTarget(TEST_FOLDER, "ImportSubfolder")
  importFolder <- createFolder(TEST_FOLDER, "ImportSubfolder")
  importWorkflow(exportFile, importFolder)

  importTree <- loadResource("./SubfolderTest", importFolder)
  # Verify subfolder structure preserved
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()
  expect_equal(nrow(importedSteps), 1,
               info = "Should have one step after import")

  # Clean up
  unlink(exportFile)
  unlink(file.path(tempdir(), "SubfolderExportLinkMapping.json"))
  unlink(file.path(tempdir(), "SubfolderExportToolMapping.json"))
})

test_that("Empty workflow export and import", {
  # Ensure TEST_FOLDER exists for individual test runs
  TEST_FOLDER <- ensureTestFolder()

  # Create empty workflow
  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                  treeName = "EmptyWorkflow")

  workflow <- getWorkflow(sourceTree)

  # Export empty workflow should warn
  expect_warning(
    exportWorkflow(workflow, workflowName = "EmptyExport",
                  targetFolder = tempdir()),
    regexp = "Cannot export empty workflow"
  )

  exportFile <- file.path(tempdir(), "EmptyExport.zip")
  # Should not create zip for empty workflow
  expect_false(file.exists(exportFile))

  # Clean up
  delete(sourceTree)
})

test_that("Complex diamond dependencies pattern", {
  # Ensure TEST_FOLDER exists for individual test runs
  TEST_FOLDER <- ensureTestFolder()

  # Create diamond pattern: Step1 → Step2 → Step4
  #                              ↘ Step3 ↗
  sourceTree <- createAnalysisTree(
    targetIdent = TEST_FOLDER,
    treeName = "DiamondDeps"
  )

  # Step 1: Initial data
  step1 <- createTestStep(
    sourceTree,
    "Step1",
    "Initial",
    paste0(TEST_FOLDER, "/DataManipulation.R"),
    paste0(TEST_FOLDER, "/data.csv")
  )
  step1Real <- step1$realise()
  step1$finishRun()

  inventory1 <- step1Real$getStepInventory()$data[[1]]
  output1 <- inventory1[inventory1$name == "chapter15_example_cleaned.rds", ]

  # Step 2: Left branch
  step2 <- createTestStep(
    sourceTree,
    "Step2",
    "Left branch",
    paste0(TEST_FOLDER, "/EDA.R")
  )
  step2$addStepRemoteFile(output1, name = "chapter15_example_cleaned.rds")
  step2Real <- step2$realise()
  step2$finishRun()

  inventory2 <- step2Real$getStepInventory()$data[[1]]
  output2 <- inventory2[inventory2$name == "EDA_table.html", ]

  # Step 3: Right branch
  step3 <- createTestStep(
    sourceTree,
    "Step3",
    "Right branch",
    paste0(TEST_FOLDER, "/test_lm_plot.R")
  )
  step3$addStepRemoteFile(output1, name = "chapter15_example_cleaned.rds")
  step3Real <- step3$realise()
  step3$finishRun()

  inventory3 <- step3Real$getStepInventory()$data[[1]]
  output3 <- inventory3[inventory3$name == "model_fit.html", ]

  # Step 4: Merge point
  step4 <- createTestStep(
    sourceTree,
    "Step4",
    "Merge",
    paste0(TEST_FOLDER, "/report.R")
  )
  step4$addStepRemoteFile(output2, name = "EDA_table.html")
  step4$addStepRemoteFile(output3, name = "model_fit.html")
  step4Real <- step4$realise()
  step4$finishRun()

  # Export
  workflow <- getWorkflow(sourceTree)
  exportWorkflow(
    workflow,
    workflowName = "DiamondExport",
    targetFolder = tempdir()
  )

  exportFile <- file.path(tempdir(), "DiamondExport.zip")

  # Delete source
  delete(sourceTree)

  # Import
  cleanImportTarget(TEST_FOLDER, "ImportDiamond")
  importFolder <- createFolder(TEST_FOLDER, "ImportDiamond")
  importWorkflow(exportFile, importFolder)

  importTree <- loadResource("./DiamondDeps", importFolder)
  # Verify diamond structure
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()

  # Comprehensive diagnostic output for diamond test
  cat("\n=== DIAMOND TEST DIAGNOSTIC OUTPUT ===\n")
  cat("Number of imported steps:", nrow(importedSteps), "\n")
  cat(
    "Imported step descriptions:",
    paste(importedSteps$description, collapse = ", "),
    "\n"
  )
  cat(
    "Imported step fullNames:",
    paste(importedSteps$fullName, collapse = ", "),
    "\n\n"
  )

  # Check original workflow structure
  cat("Original workflow had", nrow(workflow$df()), "steps\n")
  cat(
    "Original descriptions:",
    paste(workflow$df()$description, collapse = ", "),
    "\n\n"
  )

  # Check internal links
  cat("Original internal links:\n")
  if (!is.null(workflow$internalLinks)) {
    print(workflow$internalLinks)
  } else {
    cat("  No internal links in original\n")
  }

  cat("\nImported internal links:\n")
  if (!is.null(importedWorkflow$internalLinks)) {
    print(importedWorkflow$internalLinks)
  } else {
    cat("  No internal links in imported\n")
  }

  # Check workflow steps environment
  cat("\nWorkflow$steps environment contents:\n")
  if (!is.null(importedWorkflow$steps)) {
    cat("  Environment class:", class(importedWorkflow$steps), "\n")
    cat(
      "  Step names in environment:",
      paste(ls(importedWorkflow$steps), collapse = ", "),
      "\n"
    )
  } else {
    cat("  workflow$steps is NULL!\n")
  }

  # Examine the merge step specifically
  mergeStep <- importedSteps[importedSteps$description == "Merge", ]
  cat("\nMerge step details:\n")
  cat("  Number of rows found:", nrow(mergeStep), "\n")
  if (nrow(mergeStep) > 0) {
    cat("  fullName:", mergeStep$fullName, "\n")
    cat("  handle:", mergeStep$handle, "\n")

    if (
      !is.null(importedWorkflow$steps) &&
        exists(mergeStep$fullName, envir = importedWorkflow$steps)
    ) {
      mergeStepEnv <- importedWorkflow$steps[[mergeStep$fullName]]
      cat("  Step environment found\n")
      cat(
        "  Environment contents:",
        paste(ls(mergeStepEnv), collapse = ", "),
        "\n"
      )

      if ("dependencies" %in% ls(mergeStepEnv)) {
        cat("  Dependencies exists\n")
        cat("  Dependencies class:", class(mergeStepEnv$dependencies), "\n")
        if (is.environment(mergeStepEnv$dependencies)) {
          cat(
            "  Dependencies contents:",
            paste(ls(mergeStepEnv$dependencies), collapse = ", "),
            "\n"
          )
        }
      } else {
        cat("  No dependencies found in step environment!\n")
      }
    } else {
      cat("  Step environment NOT found in workflow$steps!\n")
    }
  } else {
    cat("  No merge step found with description 'Merge'\n")
  }

  # Check all steps for dependencies
  cat("\nDependencies check for all steps:\n")
  for (i in seq_len(nrow(importedSteps))) {
    stepName <- importedSteps$fullName[i]
    cat("  Step:", stepName, "\n")
    if (
      !is.null(importedWorkflow$steps) &&
        exists(stepName, envir = importedWorkflow$steps)
    ) {
      stepEnv <- importedWorkflow$steps[[stepName]]
      if ("dependencies" %in% ls(stepEnv)) {
        if (is.environment(stepEnv$dependencies)) {
          dependenciesContent <- ls(stepEnv$dependencies)
          cat(
            "    Dependencies items:",
            paste(dependenciesContent, collapse = ", "),
            "\n"
          )
        } else {
          cat("    Dependencies is not an environment\n")
        }
      } else {
        cat("    No dependencies\n")
      }
    } else {
      cat("    Step environment not found\n")
    }
  }
  cat("=== END DIAGNOSTIC OUTPUT ===\n\n")

  mergeStepEnv <- importedWorkflow$steps[[mergeStep$fullName]]
  expect_true(
    is.environment(mergeStepEnv$dependencies),
    info = "Merge step should have dependencies"
  )

  # Check it has two dependencies (excluding the 'load' function)
  dependenciesItems <- setdiff(ls(mergeStepEnv$dependencies), "load")
  expect_equal(
    length(dependenciesItems),
    2,
    info = "Merge step should have two dependencies"
  )

  # Clean up
  unlink(exportFile)
  unlink(file.path(tempdir(), "DiamondExportLinkMapping.json"))
  unlink(file.path(tempdir(), "DiamondExportToolMapping.json"))
})

test_that("Invalid import scenarios", {
  # Test missing workflow.json
  # Ensure TEST_FOLDER exists for individual test runs
  TEST_FOLDER <- ensureTestFolder()

  badZip <- tempfile(fileext = ".zip")
  badWorkflowDir <- file.path(tempdir(), "BadWorkflow")
  dir.create(badWorkflowDir)
  # Create zip without workflow.json
  file.create(file.path(badWorkflowDir, "dummy.txt"))
  zip(badZip, badWorkflowDir)

  cleanImportTarget(TEST_FOLDER, "ImportBad")
  importFolder <- createFolder(TEST_FOLDER, "ImportBad")
  expect_error(
    importWorkflow(badZip, importFolder),
    regexp = "workflow.json",
    info = "Should error on missing workflow.json"
  )

  unlink(badZip)
  unlink(badWorkflowDir, recursive = TRUE)

  # Test invalid JSON in mapping file
  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                  treeName = "InvalidMapping")
  step1 <- createTestStep(sourceTree, "Step1", "Test",
                         paste0(TEST_FOLDER, "/DataManipulation.R"))
  step1$realise()

  workflow <- getWorkflow(sourceTree)
  exportWorkflow(workflow, workflowName = "InvalidJSON",
                targetFolder = tempdir())

  # Corrupt the mapping file
  mappingPath <- file.path(tempdir(), "InvalidJSONToolMapping.json")
  writeLines("{ invalid json }", mappingPath)

  cleanImportTarget(TEST_FOLDER, "ImportInvalidJSON")
  importFolder2 <- createFolder(TEST_FOLDER, "ImportInvalidJSON")
  expect_error(
    importWorkflow(file.path(tempdir(), "InvalidJSON.zip"), importFolder2),
    regexp = "Failed to parse",
    info = "Should error on invalid JSON"
  )

  # Clean up
  delete(sourceTree)
  unlink(file.path(tempdir(), "InvalidJSON.zip"))
  unlink(mappingPath)
  unlink(file.path(tempdir(), "InvalidJSONLinkMapping.json"))
})

test_that("Export and import with hierarchical steps", {
  # Create workflow with parent-child hierarchy
  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                  treeName = "HierarchicalWorkflow")

  # Create root step
  rootStep <- createTestStep(sourceTree, "RootStep", "Root level step",
                            paste0(TEST_FOLDER, "/DataManipulation.R"),
                            paste0(TEST_FOLDER, "/data.csv"))
  rootStepReal <- rootStep$realise()
  rootStep$finishRun()

  # Create child step that inherits from root
  childStep <- createTestStep(sourceTree, "ChildStep", "Child of root",
                             paste0(TEST_FOLDER, "/EDA.R"))
  childStep$setStepParent(rootStepReal$stepDf$sourceEntityId, inheritFromParent = FALSE)
  # Add dependencies on root output
  inventory1 <- rootStepReal$getStepInventory()$data[[1]]
  outputFile1 <- inventory1[
    inventory1$name == "chapter15_example_cleaned.rds",
  ]
  childStep$addStepRemoteFile(
    outputFile1,
    name = "chapter15_example_cleaned.rds"
  )
  childStepReal <- childStep$realise()
  childStep$finishRun()

  # Create grandchild step
  grandchildStep <- createTestStep(
    sourceTree,
    "GrandchildStep",
    "Grandchild step",
    paste0(TEST_FOLDER, "/test_lm_plot.R")
  )
  grandchildStep$setStepParent(
    childStepReal$stepDf$sourceEntityId,
    inheritFromParent = TRUE
  )
  # Use output from child
  inventory2 <- childStepReal$getStepInventory()$data[[1]]
  outputFile2 <- inventory2[inventory2$name == "EDA_table.html", ]
  grandchildStep$addStepRemoteFile(outputFile2, name = "EDA_table.html")
  grandchildStepReal <- grandchildStep$realise()
  grandchildStep$finishRun()

  # Create another child of root (sibling to childStep)
  siblingStep <- createTestStep(
    sourceTree,
    "SiblingStep",
    "Another child of root",
    paste0(TEST_FOLDER, "/report.R")
  )
  siblingStep$setStepParent(
    rootStepReal$stepDf$sourceEntityId,
    inheritFromParent = FALSE
  )
  siblingStep$addStepRemoteFile(
    outputFile1,
    name = "chapter15_example_cleaned.rds"
  )
  siblingStepReal <- siblingStep$realise()
  siblingStep$finishRun()

  # Export workflow
  workflow <- getWorkflow(sourceTree)
  exportWorkflow(
    workflow,
    workflowName = "HierarchyExport",
    targetFolder = tempdir()
  )

  exportFile <- file.path(tempdir(), "HierarchyExport.zip")
  expect_true(
    file.exists(exportFile),
    info = "Hierarchy export zip should be created"
  )

  # Delete source
  delete(sourceTree)

  # Import and verify hierarchy preserved
  cleanImportTarget(TEST_FOLDER, "ImportHierarchy")
  importFolder <- createFolder(TEST_FOLDER, "ImportHierarchy")
  importWorkflow(exportFile, importFolder)

  importTree <- loadResource("./HierarchicalWorkflow", importFolder)
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()

  # Check all steps imported
  expect_equal(
    nrow(importedSteps),
    4,
    info = "Should have all 4 hierarchical steps"
  )

  # Verify parent relationships preserved
  childImport <- importedSteps[importedSteps$description == "Child of root", ]
  expect_false(
    is.na(childImport$parentIdent),
    info = "Child should have parent reference"
  )

  grandchildImport <- importedSteps[
    importedSteps$description == "Grandchild step",
  ]
  expect_false(
    is.na(grandchildImport$parentIdent),
    info = "Grandchild should have parent reference"
  )

  siblingImport <- importedSteps[
    importedSteps$description == "Another child of root",
  ]
  expect_false(
    is.na(siblingImport$parentIdent),
    info = "Sibling should have parent reference"
  )

  # Check dependencies still work
  grandchildEnv <- importedWorkflow$steps[[grandchildImport$fullName]]
  expect_true(
    is.environment(grandchildEnv$dependencies),
    info = "Grandchild should have dependencies"
  )

  # Clean up
  unlink(exportFile)
  unlink(file.path(tempdir(), "HierarchyExportLinkMapping.json"))
  unlink(file.path(tempdir(), "HierarchyExportToolMapping.json"))
})

test_that("Import fails gracefully when dependencies outputs are missing", {
  # Create workflow where step2 depends on output from step1 that doesn't exist
  sourceTree <- createAnalysisTree(
    targetIdent = TEST_FOLDER,
    treeName = "MissingOutputDeps"
  )

  # Step 1: Creates a file but we'll reference a non-existent output
  step1 <- createTestStep(
    sourceTree,
    "Step1",
    "Producer step",
    paste0(TEST_FOLDER, "/DataManipulation.R"),
    paste0(TEST_FOLDER, "/data.csv")
  )
  step1Real <- step1$realise()
  step1$finishRun()

  # Get inventory but reference a file that doesn't exist
  inventory1 <- step1Real$getStepInventory()$data[[1]]

  # Create a fake output reference that doesn't actually exist
  fakeOutput <- data.frame(
    resourceId = "fake-resource-12345",
    entityId = "fake-entity-12345",
    name = "nonexistent_output.rds",
    stringsAsFactors = FALSE
  )

  # Step 2: Tries to use the non-existent output
  step2 <- createTestStep(
    sourceTree,
    "Step2",
    "Consumer step",
    paste0(TEST_FOLDER, "/EDA.R")
  )

  # Try to add non-existent file - this should warn or error
  expect_error(
    step2$addStepRemoteFile(
      fakeOutput$entityId,
      name = "nonexistent_output.rds"
    ),
    info = "Should error when trying to load non-existent resource"
  )

  # Create step2 without the bad dependencies for export test
  step2Real <- step2$realise()

  # Export should still work but import might have issues
  workflow <- getWorkflow(sourceTree)
  exportWorkflow(
    workflow,
    workflowName = "MissingOutputExport",
    targetFolder = tempdir()
  )

  exportFile <- file.path(tempdir(), "MissingOutputExport.zip")

  # Import should succeed since we didn't add the bad dependencies
  cleanImportTarget(TEST_FOLDER, "ImportMissingOutput")
  importFolder <- createFolder(TEST_FOLDER, "ImportMissingOutput")
  # Import should work fine since step2 doesn't have the bad dependencies
  expect_no_warning(
    importWorkflow(exportFile, importFolder)
  )

  # Clean up
  delete(sourceTree)
  unlink(exportFile)
  unlink(file.path(tempdir(), "MissingOutputExportLinkMapping.json"))
  unlink(file.path(tempdir(), "MissingOutputExportToolMapping.json"))
})

test_that("Multiple process configurations preserved", {

  repoVersion <- getRepositoryVersion()
  if (startsWith(repoVersion,"4.3")) {
    testthat::skip(message = "not implemented for 4.3")
  }

  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                  treeName = "MultiProcess")

  # Create step with multiple processes
  stepEnv <- createStepTemplateEnv(treeIdent = sourceTree)

  # Main process
  stepEnv$setStepRunserverLabel(Sys.getenv("R_RUNSERVER"), "Main")
  stepEnv$setStepToolLabel(Sys.getenv("R_TOOL"), "Main")
  stepEnv$setStepToolInstance(Sys.getenv("R_TOOL_INSTANCE"), "Main")

  # Post process - ensure it's properly initialized
  stepEnv$setProcessValue("PostProcess", "selected", TRUE)
  stepEnv$setStepRunserverLabel(Sys.getenv("R_RUNSERVER"), "PostProcess")
  stepEnv$setStepToolLabel(Sys.getenv("R_TOOL"), "PostProcess")
  stepEnv$setStepToolInstance(Sys.getenv("R_TOOL_INSTANCE"), "PostProcess")
  # Add a command file for PostProcess too
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/report.R"),
                           variableName = "command-file", variableProcess = "PostProcess")

  stepEnv$setStepDescription("Multi-process step")
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"),
                           variableName = "command-file")

  stepReal <- stepEnv$realise()
  stepEnv$finishRun()

  # Export
  workflow <- getWorkflow(sourceTree)
  exportWorkflow(workflow, workflowName = "MultiProcessExport",
                targetFolder = tempdir())

  exportFile <- file.path(tempdir(), "MultiProcessExport.zip")

  # Delete source
  delete(sourceTree)

  # Import
  cleanImportTarget(TEST_FOLDER, "ImportMultiProcess")
  importFolder <- createFolder(TEST_FOLDER, "ImportMultiProcess")
  importWorkflow(exportFile, importFolder)

  importTree <- loadResource("./MultiProcess", importFolder)
  # Verify processes preserved
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()

  processes <- importedSteps$processes[[1]]
  expect_true(nrow(processes) >= 2,
              info = "Should have multiple processes")
  expect_true("PostProcess" %in% processes$name,
              info = "PostProcess should be preserved")

  # Clean up
  unlink(exportFile)
  unlink(file.path(tempdir(), "MultiProcessExportLinkMapping.json"))
  unlink(file.path(tempdir(), "MultiProcessExportToolMapping.json"))
})
