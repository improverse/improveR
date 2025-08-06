# Test suite for import/export workflow functionality
# Tests the complete round-trip of exporting and importing workflows
# including dependency preservation, file handling, and mapping configurations

Sys.setenv(TEST_NAME="importExport")

# Helper function to set up test environment
setupTestEnvironment <- function() {
  clearConnectionData()
  Sys.setenv(IMPROVER_TEST_REPLAY="T")
  improveConnect()
  setEditable(T)
  TEST_FOLDER <- workflowFilesSetup()
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

  expect_equal(nrow(originalSteps), nrow(importedSteps),
               info = "Step count should match")

  # Check step descriptions preserved
  expect_setequal(originalSteps$description, importedSteps$description,
                  info = "Step descriptions should be preserved")

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
  TEST_FOLDER <- setupTestEnvironment()
  expect_false(Sys.getenv("IMPROVER_TOKEN") == "")
  assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
})

test_that("Basic export and import round trip", {
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
  importFolder <- createFolder(TEST_FOLDER, "ImportTarget1")
  importWorkflow(exportFile, importFolder)

  importTree <- loadResource("./ExportSource", importFolder)
  # Verify structure preserved
  importedWorkflow <- verifyWorkflowStructure(workflow, importTree)

  # Clean up
  unlink(exportFile)
  unlink(file.path(tempdir(), "TestBasicExportLinkMapping.json"))
  unlink(file.path(tempdir(), "TestBasicExportToolMapping.json"))
})

test_that("Export and import with linear dependencies", {
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
  importFolder <- createFolder(TEST_FOLDER, "ImportLinear")
  importWorkflow(exportFile, importFolder)

  importTree <- loadResource("./LinearDependencies",importFolder)
  # Verify dependencies preserved
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()

  # Check that Step3 depends on Step2
  step3Import <- importedSteps[importedSteps$description == "Report", ]
  step3ImportEnv <- importedWorkflow$steps[[step3Import$fullName]]
  expect_true(is.environment(step3ImportEnv$lineage),
              info = "Step3 should have dependencies")

  # Clean up
  unlink(exportFile)
  unlink(file.path(tempdir(), "LinearDepsLinkMapping.json"))
  unlink(file.path(tempdir(), "LinearDepsToolMapping.json"))
})

test_that("Export and import with branching dependencies", {
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

  importFolder <- createFolder(TEST_FOLDER, "ImportBranching")
  importWorkflow(exportFile, importFolder)

  importTree <- loadResource("./BranchingDeps", importFolder)
  # Verify both branches have correct dependency
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()

  branchA <- importedSteps[importedSteps$description == "Branch A", ]
  branchB <- importedSteps[importedSteps$description == "Branch B", ]
  branchAEnv <- importedWorkflow$steps[[branchA$fullName]]
  branchBEnv <- importedWorkflow$steps[[branchB$fullName]]

  expect_true(is.environment(branchAEnv$lineage), info = "Branch A should have dependency")
  expect_true(is.environment(branchBEnv$lineage), info = "Branch B should have dependency")

  # Clean up
  unlink(exportFile)
  unlink(file.path(tempdir(), "BranchingDepsLinkMapping.json"))
  unlink(file.path(tempdir(), "BranchingDepsToolMapping.json"))
})

test_that("Tool mapping validation and application", {
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
  importFolder <- createFolder(TEST_FOLDER, "ImportToolValid")
  expect_silent(importWorkflow(file.path(tempdir(), "ToolTest.zip"), importFolder))

  # Test with invalid tool mapping
  toolMapping$runserverLabel <- "nonexistent-server"
  jsonlite::write_json(toolMapping, toolMappingPath, pretty = TRUE)

  importFolder2 <- createFolder(TEST_FOLDER, "ImportToolInvalid")
  expect_warning(
    importWorkflow(file.path(tempdir(), "ToolTest.zip"), importFolder2),
    regexp = "Runserver not found",
    info = "Should warn about invalid runserver"
  )

  # Clean up
  delete(sourceTree)
  unlink(file.path(tempdir(), "ToolTest.zip"))
  unlink(toolMappingPath)
  unlink(file.path(tempdir(), "ToolTestLinkMapping.json"))
})

test_that("Link mapping with existing resources", {
  # Create some resources to map to
  existingFile <- createFile(TEST_FOLDER,
                            fileName = "existing_data.csv",
                            localPath = file.path(TEST_FOLDER, "data.csv"))

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

  # Import with mapping
  importFolder <- createFolder(TEST_FOLDER, "ImportLinkMapped")
  expect_message(
    importWorkflow(file.path(tempdir(), "LinkTest.zip"), importFolder),
    regexp = "Validated resource",
    info = "Should validate mapped resources"
  )

  # Test with invalid link mapping
  linkMapping$ident[1] <- "invalid-entity-id"
  jsonlite::write_json(linkMapping, linkMappingPath, pretty = TRUE)

  importFolder2 <- createFolder(TEST_FOLDER, "ImportLinkInvalid")
  expect_warning(
    importWorkflow(file.path(tempdir(), "LinkTest.zip"), importFolder2),
    regexp = "Resource not found|Invalid ident",
    info = "Should warn about invalid resource mapping"
  )

  # Clean up
  delete(sourceTree)
  unlink(file.path(tempdir(), "LinkTest.zip"))
  unlink(linkMappingPath)
  unlink(file.path(tempdir(), "LinkTestToolMapping.json"))
})

test_that("Export and import with subfolders", {
  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                  treeName = "SubfolderTest")

  # Create step with files in subfolders
  step1 <- createTestStep(sourceTree, "Step1", "Subfolder test",
                         paste0(TEST_FOLDER, "/DataManipulation.R"))
  # Add file to subfolder
  step1$addStepRemoteFile(paste0(TEST_FOLDER, "/data.csv"),
                         name = "input/data.csv", asLink = FALSE)
  step1$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.Rmd"),
                         name = "docs/readme.Rmd", asLink = FALSE)
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
  # Create empty workflow
  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                  treeName = "EmptyWorkflow")

  workflow <- getWorkflow(sourceTree)

  # Export empty workflow
  exportWorkflow(workflow, workflowName = "EmptyExport",
                targetFolder = tempdir())

  exportFile <- file.path(tempdir(), "EmptyExport.zip")
  expect_true(file.exists(exportFile),
              info = "Should create zip even for empty workflow")

  # Import
  importFolder <- createFolder(TEST_FOLDER, "ImportEmpty")
  expect_silent(importWorkflow(exportFile, importFolder))

  importTree <- loadResource("./EmptyWorkflow", importFolder)
  # Verify empty workflow imported
  importedWorkflow <- getWorkflow(importTree)
  expect_equal(nrow(importedWorkflow$df()), 0,
               info = "Imported workflow should be empty")

  # Clean up
  delete(sourceTree)
  unlink(exportFile)
  # Note: Mapping files might not exist for empty workflow
  suppressWarnings({
    unlink(file.path(tempdir(), "EmptyExportLinkMapping.json"))
    unlink(file.path(tempdir(), "EmptyExportToolMapping.json"))
  })
})

test_that("Complex diamond dependency pattern", {
  # Create diamond pattern: Step1 → Step2 → Step4
  #                              ↘ Step3 ↗
  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                  treeName = "DiamondDeps")

  # Step 1: Initial data
  step1 <- createTestStep(sourceTree, "Step1", "Initial",
                         paste0(TEST_FOLDER, "/DataManipulation.R"),
                         paste0(TEST_FOLDER, "/data.csv"))
  step1Real <- step1$realise()
  step1$finishRun()

  inventory1 <- step1Real$getStepInventory()$data[[1]]
  output1 <- inventory1[inventory1$name == "chapter15_example_cleaned.rds", ]

  # Step 2: Left branch
  step2 <- createTestStep(sourceTree, "Step2", "Left branch",
                         paste0(TEST_FOLDER, "/EDA.R"))
  step2$addStepRemoteFile(output1, name = "chapter15_example_cleaned.rds")
  step2Real <- step2$realise()
  step2$finishRun()

  inventory2 <- step2Real$getStepInventory()$data[[1]]
  output2 <- inventory2[inventory2$name == "EDA_table.html", ]

  # Step 3: Right branch
  step3 <- createTestStep(sourceTree, "Step3", "Right branch",
                         paste0(TEST_FOLDER, "/test_lm_plot.R"))
  step3$addStepRemoteFile(output1, name = "chapter15_example_cleaned.rds")
  step3Real <- step3$realise()
  step3$finishRun()

  inventory3 <- step3Real$getStepInventory()$data[[1]]
  output3 <- inventory3[inventory3$name == "model_fit.html", ]

  # Step 4: Merge point
  step4 <- createTestStep(sourceTree, "Step4", "Merge",
                         paste0(TEST_FOLDER, "/report.R"))
  step4$addStepRemoteFile(output2, name = "EDA_table.html")
  step4$addStepRemoteFile(output3, name = "model_fit.html")
  step4Real <- step4$realise()
  step4$finishRun()

  # Export
  workflow <- getWorkflow(sourceTree)
  exportWorkflow(workflow, workflowName = "DiamondExport",
                targetFolder = tempdir())

  exportFile <- file.path(tempdir(), "DiamondExport.zip")

  # Delete source
  delete(sourceTree)

  # Import
  importFolder <- createFolder(TEST_FOLDER, "ImportDiamond")
  importWorkflow(exportFile, importFolder)

  importTree <- loadResource("./DiamondDeps", importFolder)
  # Verify diamond structure
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()

  mergeStep <- importedSteps[importedSteps$description == "Merge", ]
  mergeStepEnv <- importedWorkflow$steps[[mergeStep$fullName]]
  expect_true(is.environment(mergeStepEnv$lineage),
              info = "Merge step should have dependencies")

  # Check it has two dependencies
  expect_equal(length(ls(mergeStepEnv$lineage)), 2,
               info = "Merge step should have two dependencies")

  # Clean up
  unlink(exportFile)
  unlink(file.path(tempdir(), "DiamondExportLinkMapping.json"))
  unlink(file.path(tempdir(), "DiamondExportToolMapping.json"))
})

test_that("Invalid import scenarios", {
  # Test missing workflow.json
  badZip <- tempfile(fileext = ".zip")
  dir.create(tempdir(), "BadWorkflow")
  # Create zip without workflow.json
  file.create(file.path(tempdir(), "BadWorkflow", "dummy.txt"))
  zip(badZip, file.path(tempdir(), "BadWorkflow"))

  importFolder <- createFolder(TEST_FOLDER, "ImportBad")
  expect_error(
    importWorkflow(badZip, importFolder),
    regexp = "workflow.json",
    info = "Should error on missing workflow.json"
  )

  unlink(badZip)
  unlink(file.path(tempdir(), "BadWorkflow"), recursive = TRUE)

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

test_that("Multiple process configurations preserved", {
  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                  treeName = "MultiProcess")

  # Create step with multiple processes
  stepEnv <- createStepTemplateEnv(treeIdent = sourceTree)

  # Main process
  stepEnv$setStepRunserverLabel(Sys.getenv("R_RUNSERVER"), "Main")
  stepEnv$setStepToolLabel(Sys.getenv("R_TOOL"), "Main")
  stepEnv$setStepToolInstance(Sys.getenv("R_TOOL_INSTANCE"), "Main")

  # Post process
  stepEnv$setProcessValue("PostProcess", "selected", TRUE)
  stepEnv$setStepRunserverLabel(Sys.getenv("R_RUNSERVER"), "PostProcess")
  stepEnv$setStepToolLabel(Sys.getenv("R_TOOL"), "PostProcess")
  stepEnv$setStepToolInstance(Sys.getenv("R_TOOL_INSTANCE"), "PostProcess")

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
