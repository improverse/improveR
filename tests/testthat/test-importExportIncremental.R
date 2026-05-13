# Incremental import/export tests
# Each test focuses on exactly one behavior, building up from simplest to complex
# Run with: Rscript run-tests-unified.R importExportIncremental

Sys.setenv(TEST_NAME="importExportIncremental")

ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME="importExportIncremental")
  if (!exists("TEST_FOLDER") || is.null(TEST_FOLDER)) {
    setEditable(TRUE)
    TEST_FOLDER <- workflowFilesSetup()
    assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
    return(TEST_FOLDER)
  }
  return(TEST_FOLDER)
}

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

# Helper: export, delete source, import, return imported workflow
roundTrip <- function(workflow, sourceTree, workflowName, importFolderName) {
  TEST_FOLDER <- ensureTestFolder()

  exportWorkflow(workflow, workflowName = workflowName,
                targetFolder = tempdir())
  exportFile <- file.path(tempdir(), paste0(workflowName, ".zip"))
  expect_true(file.exists(exportFile), info = "Export zip should exist")

  # Delete source so import can't cheat
  delete(sourceTree)

  # Import
  importFolder <- createFolder(TEST_FOLDER, importFolderName)
  importWorkflow(exportFile, importFolder)

  # Cleanup export artifacts
  unlink(exportFile)
  unlink(file.path(tempdir(), paste0(workflowName, "LinkMapping.json")))
  unlink(file.path(tempdir(), paste0(workflowName, "ToolMapping.json")))

  return(importFolder)
}


test_that("Setup", {
  Sys.setenv(IMPROVER_TEST_REPLAY="T")
  if (!improveConnected()) {
    tryCatch(improveConnect(), error = function(e) {})
  }
  setEditable(TRUE)
  TEST_FOLDER <- workflowFilesSetup()
  expect_false(Sys.getenv("IMPROVER_TOKEN") == "")
  assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
})


# ============================================================
# Level 1: Single step
# ============================================================
test_that("L1: Single step export/import", {
  TEST_FOLDER <- ensureTestFolder()

  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                   treeName = "L1_Single")
  step1 <- createTestStep(sourceTree, "Step1", "Only step",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1$realise()
  step1$finishRun()

  workflow <- getWorkflow(sourceTree)
  originalSteps <- workflow$df()
  cat("\n[L1] Original steps:", nrow(originalSteps), "\n")

  importFolder <- roundTrip(workflow, sourceTree, "L1Single", "ImportL1")
  importTree <- loadResource("./L1_Single", importFolder)
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()

  cat("[L1] Imported steps:", nrow(importedSteps), "\n")
  expect_equal(nrow(importedSteps), 1)
  expect_equal(importedSteps$description, "Only step")
})


# ============================================================
# Level 2: Two independent steps (no links)
# ============================================================
test_that("L2: Two independent steps export/import", {
  TEST_FOLDER <- ensureTestFolder()

  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                   treeName = "L2_TwoIndep")
  step1 <- createTestStep(sourceTree, "Step1", "First independent",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1$realise()
  step1$finishRun()

  step2 <- createTestStep(sourceTree, "Step2", "Second independent",
                          paste0(TEST_FOLDER, "/EDA.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step2$realise()
  step2$finishRun()

  workflow <- getWorkflow(sourceTree)
  originalSteps <- workflow$df()
  cat("\n[L2] Original steps:", nrow(originalSteps), "\n")
  cat("[L2] internalLinks:", if (is.null(workflow$internalLinks)) "NULL" else nrow(workflow$internalLinks), "\n")

  importFolder <- roundTrip(workflow, sourceTree, "L2TwoIndep", "ImportL2")
  importTree <- loadResource("./L2_TwoIndep", importFolder)
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()

  cat("[L2] Imported steps:", nrow(importedSteps), "\n")
  expect_equal(nrow(importedSteps), 2)
  expect_setequal(importedSteps$description, c("First independent", "Second independent"))
})


# ============================================================
# Level 3: Two steps, second uses first's output (internal link)
# ============================================================
test_that("L3: Two steps with internal link", {
  TEST_FOLDER <- ensureTestFolder()

  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                   treeName = "L3_Linked")

  # Step 1: produces output
  step1 <- createTestStep(sourceTree, "Step1", "Producer",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1Real <- step1$realise()
  step1$finishRun()

  # Get step1's output file
  inventory1 <- step1Real$getStepInventory()$data[[1]]
  cat("\n[L3] Step1 inventory:\n")
  print(inventory1[, c("name", "entityId")])
  outputFile1 <- inventory1[inventory1$name == "chapter15_example_cleaned.rds", ]

  # Step 2: uses step1's output as link
  step2 <- createTestStep(sourceTree, "Step2", "Consumer",
                          paste0(TEST_FOLDER, "/EDA.R"))
  step2$addStepRemoteFile(outputFile1, name = "chapter15_example_cleaned.rds")
  step2Real <- step2$realise()
  step2$finishRun()

  # Get workflow
  workflow <- getWorkflow(sourceTree)
  expect_true(!is.null(workflow$internalLinks) && nrow(workflow$internalLinks) > 0,
              info = "Source workflow should have internal links")

  # Export, delete, import
  exportWorkflow(workflow, workflowName = "L3Linked", targetFolder = tempdir())
  exportFile <- file.path(tempdir(), "L3Linked.zip")
  expect_true(file.exists(exportFile))

  # Verify internalLinks.json in zip
  zipContents <- zip::zip_list(exportFile)
  expect_true(any(grepl("internalLinks.json", zipContents$filename)),
              info = "Export should contain internalLinks.json")

  delete(sourceTree)
  importFolder <- createFolder(TEST_FOLDER, "ImportL3")
  importWorkflow(exportFile, importFolder)

  importTree <- loadResource("./L3_Linked", importFolder)
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()

  cat("[L3] Imported steps:", nrow(importedSteps), "\n")
  expect_equal(nrow(importedSteps), 2)
  expect_setequal(importedSteps$description, c("Producer", "Consumer"))

  # Check the consumer step has a link to the producer's output
  consumerStep <- importedSteps[importedSteps$description == "Consumer", ]
  consumerEnv <- importedWorkflow$steps[[consumerStep$fullName]]
  consumerResource <- loadResource(consumerEnv$stepDf$sourceEntityId)
  consumerChildren <- loadChildResources(consumerResource)$data[[1]]
  hasOutputLink <- any(consumerChildren$name == "chapter15_example_cleaned.rds" &
                       consumerChildren$nodeType == "Link")
  expect_true(hasOutputLink,
              info = "Consumer step should have a link to producer's output file")

  # Verify dependencies are detected by getWorkflow
  consumerDeps <- setdiff(ls(consumerEnv$dependencies), "load")
  expect_true(length(consumerDeps) > 0,
              info = "Consumer step should have dependency on producer step")

  # Cleanup
  unlink(exportFile)
  unlink(file.path(tempdir(), paste0("L3Linked", "LinkMapping.json")))
  unlink(file.path(tempdir(), paste0("L3Linked", "ToolMapping.json")))
})


# ============================================================
# Level 4: Linear chain - Step1 -> Step2 -> Step3
# ============================================================
test_that("L4: Three-step linear chain with internal links", {
  TEST_FOLDER <- ensureTestFolder()

  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                   treeName = "L4_Linear")

  # Step 1
  step1 <- createTestStep(sourceTree, "Step1", "DataPrep",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1Real <- step1$realise()
  step1$finishRun()

  # Get step1 output
  inv1 <- step1Real$getStepInventory()$data[[1]]
  output1 <- inv1[inv1$name == "chapter15_example_cleaned.rds", ]

  # Step 2: uses step1's output
  step2 <- createTestStep(sourceTree, "Step2", "Analysis",
                          paste0(TEST_FOLDER, "/EDA.R"))
  step2$addStepRemoteFile(output1, name = "chapter15_example_cleaned.rds")
  step2Real <- step2$realise()
  step2$finishRun()

  # Get step2 output
  inv2 <- step2Real$getStepInventory()$data[[1]]
  output2 <- inv2[inv2$name == "EDA_table.html", ]

  # Step 3: uses step2's output
  step3 <- createTestStep(sourceTree, "Step3", "Report",
                          paste0(TEST_FOLDER, "/report.R"))
  step3$addStepRemoteFile(output2, name = "EDA_table.html")
  step3Real <- step3$realise()
  step3$finishRun()

  # Export, delete, import
  workflow <- getWorkflow(sourceTree)
  expect_true(!is.null(workflow$internalLinks) && nrow(workflow$internalLinks) >= 2,
              info = "Should have at least 2 internal links")

  exportWorkflow(workflow, workflowName = "L4Linear", targetFolder = tempdir())
  exportFile <- file.path(tempdir(), "L4Linear.zip")
  delete(sourceTree)

  importFolder <- createFolder(TEST_FOLDER, "ImportL4")
  importWorkflow(exportFile, importFolder)

  importTree <- loadResource("./L4_Linear", importFolder)
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()

  expect_equal(nrow(importedSteps), 3)

  # Verify Step2 has link to Step1's output
  step2Import <- importedSteps[importedSteps$description == "Analysis", ]
  step2Resource <- loadResource(
    importedWorkflow$steps[[step2Import$fullName]]$stepDf$sourceEntityId
  )
  step2Children <- loadChildResources(step2Resource)$data[[1]]
  expect_true(
    any(step2Children$name == "chapter15_example_cleaned.rds" & step2Children$nodeType == "Link"),
    info = "Step2 should have link to Step1's output"
  )

  # Verify Step3 has link to Step2's output
  step3Import <- importedSteps[importedSteps$description == "Report", ]
  step3Resource <- loadResource(
    importedWorkflow$steps[[step3Import$fullName]]$stepDf$sourceEntityId
  )
  step3Children <- loadChildResources(step3Resource)$data[[1]]
  expect_true(
    any(step3Children$name == "EDA_table.html" & step3Children$nodeType == "Link"),
    info = "Step3 should have link to Step2's output"
  )

  # Cleanup
  unlink(exportFile)
  unlink(file.path(tempdir(), "L4LinearLinkMapping.json"))
  unlink(file.path(tempdir(), "L4LinearToolMapping.json"))
})


# ============================================================
# Level 5: Branching - Step1 -> Step2 AND Step1 -> Step3
# ============================================================
test_that("L5: Branching dependencies export/import", {
  TEST_FOLDER <- ensureTestFolder()

  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                   treeName = "L5_Branch")

  # Step 1: Common data source
  step1 <- createTestStep(sourceTree, "Step1", "DataSource",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1Real <- step1$realise()
  step1$finishRun()

  inv1 <- step1Real$getStepInventory()$data[[1]]
  output1 <- inv1[inv1$name == "chapter15_example_cleaned.rds", ]

  # Step 2: Branch A - uses step1's output
  step2 <- createTestStep(sourceTree, "Step2", "BranchA",
                          paste0(TEST_FOLDER, "/EDA.R"))
  step2$addStepRemoteFile(output1, name = "chapter15_example_cleaned.rds")
  step2Real <- step2$realise()
  step2$finishRun()

  # Step 3: Branch B - also uses step1's output
  step3 <- createTestStep(sourceTree, "Step3", "BranchB",
                          paste0(TEST_FOLDER, "/test_lm_plot.R"))
  step3$addStepRemoteFile(output1, name = "chapter15_example_cleaned.rds")
  step3Real <- step3$realise()
  step3$finishRun()

  workflow <- getWorkflow(sourceTree)
  expect_true(!is.null(workflow$internalLinks) && nrow(workflow$internalLinks) >= 2,
              info = "Should have at least 2 internal links")

  importFolder <- roundTrip(workflow, sourceTree, "L5Branch", "ImportL5")
  importTree <- loadResource("./L5_Branch", importFolder)
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()

  expect_equal(nrow(importedSteps), 3)

  # Check Branch A has dependency on DataSource
  branchA <- importedSteps[importedSteps$description == "BranchA", ]
  branchAEnv <- importedWorkflow$steps[[branchA$fullName]]
  branchADeps <- setdiff(ls(branchAEnv$dependencies), "load")
  expect_true(length(branchADeps) > 0,
              info = "Branch A should have dependency on DataSource")

  # Check Branch B has dependency on DataSource
  branchB <- importedSteps[importedSteps$description == "BranchB", ]
  branchBEnv <- importedWorkflow$steps[[branchB$fullName]]
  branchBDeps <- setdiff(ls(branchBEnv$dependencies), "load")
  expect_true(length(branchBDeps) > 0,
              info = "Branch B should have dependency on DataSource")
})


# ============================================================
# Special characters in filenames
# ============================================================
test_that("Special characters in filenames survive export/import", {
  TEST_FOLDER <- ensureTestFolder()

  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                   treeName = "SpecialChars")

  stepEnv <- createStepTemplateEnv(treeIdent = sourceTree)
  stepEnv$setStepRunserverLabel(Sys.getenv("R_RUNSERVER"))
  stepEnv$setStepToolLabel(Sys.getenv("R_TOOL"))
  stepEnv$setStepToolInstance(Sys.getenv("R_TOOL_INSTANCE"))
  stepEnv$setStepDescription("Special chars test")
  stepEnv$setStepRationale("Test special characters in filenames")

  # Add the same file multiple times with different special-character names
  dataFile <- paste0(TEST_FOLDER, "/data.csv")
  commandFile <- paste0(TEST_FOLDER, "/DataManipulation.R")

  stepEnv$addStepRemoteFile(commandFile, variableName = "command-file")
  stepEnv$addStepRemoteFile(dataFile, name = "file with spaces.csv")
  stepEnv$addStepRemoteFile(dataFile, name = "file(parens).csv")
  stepEnv$addStepRemoteFile(dataFile, name = "file#hash.csv")
  stepEnv$addStepRemoteFile(dataFile, name = "file&amp.csv")
  stepEnv$addStepRemoteFile(dataFile, name = "file%percent.csv")

  stepEnv$realise()
  stepEnv$finishRun()

  workflow <- getWorkflow(sourceTree)
  originalSteps <- workflow$df()
  originalRemoteFiles <- workflow$steps[[originalSteps$fullName[1]]]$stepDf$remoteFiles[[1]]
  specialNames <- c("file with spaces.csv", "file(parens).csv",
                     "file#hash.csv", "file&amp.csv", "file%percent.csv")

  cat("\n[SpecialChars] Original file names:", paste(originalRemoteFiles$name, collapse = ", "), "\n")

  importFolder <- roundTrip(workflow, sourceTree, "SpecialChars", "ImportSpecialChars")
  importTree <- loadResource("./SpecialChars", importFolder)
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()

  expect_equal(nrow(importedSteps), 1)

  importedRemoteFiles <- importedWorkflow$steps[[importedSteps$fullName[1]]]$stepDf$remoteFiles[[1]]
  cat("[SpecialChars] Imported file names:", paste(importedRemoteFiles$name, collapse = ", "), "\n")

  # Names may have ./ prefix after roundtrip - strip for comparison
  importedNames <- gsub("^\\./", "", importedRemoteFiles$name)
  for (sn in specialNames) {
    expect_true(sn %in% importedNames,
                info = paste("Filename should survive roundtrip:", sn))
  }
})

test_that("Comma in filename survives export/import", {
  TEST_FOLDER <- ensureTestFolder()

  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                   treeName = "CommaChars")

  stepEnv <- createStepTemplateEnv(treeIdent = sourceTree)
  stepEnv$setStepRunserverLabel(Sys.getenv("R_RUNSERVER"))
  stepEnv$setStepToolLabel(Sys.getenv("R_TOOL"))
  stepEnv$setStepToolInstance(Sys.getenv("R_TOOL_INSTANCE"))
  stepEnv$setStepDescription("Comma test")
  stepEnv$setStepRationale("Test comma in filename")

  commandFile <- paste0(TEST_FOLDER, "/DataManipulation.R")
  dataFile <- paste0(TEST_FOLDER, "/data.csv")

  stepEnv$addStepRemoteFile(commandFile, variableName = "command-file")
  stepEnv$addStepRemoteFile(dataFile, name = "data, 2024.csv")

  stepEnv$realise()
  stepEnv$finishRun()

  workflow <- getWorkflow(sourceTree)
  originalSteps <- workflow$df()
  originalRemoteFiles <- workflow$steps[[originalSteps$fullName[1]]]$stepDf$remoteFiles[[1]]
  cat("\n[Comma] Original file names:", paste(originalRemoteFiles$name, collapse = " | "), "\n")

  importFolder <- roundTrip(workflow, sourceTree, "CommaChars", "ImportCommaChars")
  importTree <- loadResource("./CommaChars", importFolder)
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()

  expect_equal(nrow(importedSteps), 1)

  importedRemoteFiles <- importedWorkflow$steps[[importedSteps$fullName[1]]]$stepDf$remoteFiles[[1]]
  importedNames <- gsub("^\\./", "", importedRemoteFiles$name)
  cat("[Comma] Imported file names:", paste(importedNames, collapse = " | "), "\n")

  expect_true("data, 2024.csv" %in% importedNames,
              info = "Filename with comma should survive roundtrip")
})


# ============================================================
# Comma in outside link name (aggregation edge case)
# ============================================================
test_that("Comma in outside link name survives when two steps share a link", {
  TEST_FOLDER <- ensureTestFolder()

  # Create a file outside the workflow to use as an outside link
  outsideFile <- createFile(TEST_FOLDER,
                           fileName = "shared, data.csv",
                           localPath = file.path(
                             tempdir(), "ExampleWorkflow", "data.csv"
                           ))
  # Fallback: if ExampleWorkflow doesn't exist, use the workflowFilesSetup data
  if (is.null(outsideFile)) {
    outsideFile <- loadResource(paste0(TEST_FOLDER, "/data.csv"))
  }

  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                   treeName = "CommaLink")

  # Step 1 links to the outside file
  step1 <- createStepTemplateEnv(treeIdent = sourceTree)
  step1$setStepRunserverLabel(Sys.getenv("R_RUNSERVER"))
  step1$setStepToolLabel(Sys.getenv("R_TOOL"))
  step1$setStepToolInstance(Sys.getenv("R_TOOL_INSTANCE"))
  step1$setStepDescription("Step with outside link")
  step1$setStepRationale("Test outside link with comma")
  step1$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"),
                          variableName = "command-file")
  step1$addStepRemoteFile(outsideFile, name = "shared, data.csv")
  step1$realise()
  step1$finishRun()

  # Step 2 also links to the same outside file
  step2 <- createStepTemplateEnv(treeIdent = sourceTree)
  step2$setStepRunserverLabel(Sys.getenv("R_RUNSERVER"))
  step2$setStepToolLabel(Sys.getenv("R_TOOL"))
  step2$setStepToolInstance(Sys.getenv("R_TOOL_INSTANCE"))
  step2$setStepDescription("Another step with same link")
  step2$setStepRationale("Test shared outside link")
  step2$addStepRemoteFile(paste0(TEST_FOLDER, "/EDA.R"),
                          variableName = "command-file")
  step2$addStepRemoteFile(outsideFile, name = "shared, data.csv")
  step2$realise()
  step2$finishRun()

  workflow <- getWorkflow(sourceTree)

  # Export
  exportWorkflow(workflow, workflowName = "CommaLink",
                targetFolder = tempdir())

  # Check what the LinkMapping looks like
  linkMappingPath <- file.path(tempdir(), "CommaLinkLinkMapping.json")
  if (file.exists(linkMappingPath)) {
    linkMapping <- jsonlite::read_json(linkMappingPath, simplifyVector = TRUE)
    cat("\n[CommaLink] LinkMapping names:", paste(linkMapping$name, collapse = " | "), "\n")
  }

  exportFile <- file.path(tempdir(), "CommaLink.zip")
  expect_true(file.exists(exportFile))

  delete(sourceTree)

  importFolder <- createFolder(TEST_FOLDER, "ImportCommaLink")
  importWorkflow(exportFile, importFolder)

  importTree <- loadResource("./CommaLink", importFolder)
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()

  expect_equal(nrow(importedSteps), 2)

  # Check both steps have a file with "shared, data.csv" name
  for (i in 1:nrow(importedSteps)) {
    stepEnv <- importedWorkflow$steps[[importedSteps$fullName[i]]]
    rf <- stepEnv$stepDf$remoteFiles[[1]]
    importedNames <- gsub("^\\./", "", rf$name)
    cat("[CommaLink] Step", i, "files:", paste(importedNames, collapse = " | "), "\n")
    expect_true("shared, data.csv" %in% importedNames,
                info = paste("Step", i, "should have 'shared, data.csv'"))
  }

  # Cleanup
  unlink(exportFile)
  unlink(linkMappingPath)
  unlink(file.path(tempdir(), "CommaLinkToolMapping.json"))
})


# ============================================================
# All combinations: files (asLink=FALSE) + outside links + internal links
# ============================================================
test_that("Mix of copied files, outside links, and internal links", {
  TEST_FOLDER <- ensureTestFolder()

  sourceTree <- createAnalysisTree(targetIdent = TEST_FOLDER,
                                   treeName = "MixedCombo")

  # Step 1: has a copied file (asLink=FALSE) and an outside link (asLink=TRUE)
  step1 <- createTestStep(sourceTree, "Step1", "Producer with mixed files",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  # Copied file (asLink=FALSE)
  step1$addStepRemoteFile(paste0(TEST_FOLDER, "/EDA.R"),
                          name = "copied_script.R", asLink = FALSE)
  step1Real <- step1$realise()
  step1$finishRun()

  # Get step1's actual output file (generated by DataManipulation.R)
  inventory1 <- step1Real$getStepInventory()$data[[1]]
  cat("\n[MixedCombo] Step1 inventory:", paste(inventory1$name, collapse = ", "), "\n")
  outputFile1 <- inventory1[inventory1$name == "chapter15_example_cleaned.rds", ]

  # Step 2: depends on Step 1 (internal link) + has its own outside link
  step2 <- createTestStep(sourceTree, "Step2", "Consumer with dependency",
                          paste0(TEST_FOLDER, "/EDA.R"))
  # Internal link (from Step 1's actual output)
  step2$addStepRemoteFile(outputFile1, name = "chapter15_example_cleaned.rds")
  # Outside link
  step2$addStepRemoteFile(paste0(TEST_FOLDER, "/report.R"),
                          name = "outside_report.R")
  step2Real <- step2$realise()
  step2$finishRun()

  workflow <- getWorkflow(sourceTree)
  originalSteps <- workflow$df()
  cat("\n[MixedCombo] Original steps:", nrow(originalSteps), "\n")
  cat("[MixedCombo] Internal links:",
      if (is.null(workflow$internalLinks)) "NULL" else nrow(workflow$internalLinks), "\n")

  importFolder <- roundTrip(workflow, sourceTree, "MixedCombo", "ImportMixedCombo")
  importTree <- loadResource("./MixedCombo", importFolder)
  importedWorkflow <- getWorkflow(importTree)
  importedSteps <- importedWorkflow$df()

  expect_equal(nrow(importedSteps), 2)

  # Check Step 1 (producer): should have command-file, linked_data, copied_script
  producer <- importedSteps[importedSteps$description == "Producer with mixed files", ]
  producerEnv <- importedWorkflow$steps[[producer$fullName]]
  producerFiles <- producerEnv$stepDf$remoteFiles[[1]]
  producerNames <- gsub("^\\./", "", producerFiles$name)
  cat("[MixedCombo] Producer files:", paste(producerNames, collapse = " | "), "\n")
  expect_true("data.csv" %in% producerNames,
              info = "Producer should have outside link")
  expect_true("copied_script.R" %in% producerNames,
              info = "Producer should have copied file")

  # Check Step 2 (consumer): should have command-file, data.csv (internal), outside_report.R
  consumer <- importedSteps[importedSteps$description == "Consumer with dependency", ]
  consumerEnv <- importedWorkflow$steps[[consumer$fullName]]
  consumerFiles <- consumerEnv$stepDf$remoteFiles[[1]]
  consumerNames <- gsub("^\\./", "", consumerFiles$name)
  cat("[MixedCombo] Consumer files:", paste(consumerNames, collapse = " | "), "\n")
  expect_true("chapter15_example_cleaned.rds" %in% consumerNames,
              info = "Consumer should have internal link file")
  expect_true("outside_report.R" %in% consumerNames,
              info = "Consumer should have outside link")

  # Check that consumer has dependency on producer
  consumerDeps <- setdiff(ls(consumerEnv$dependencies), "load")
  expect_true(length(consumerDeps) > 0,
              info = "Consumer should have dependency on Producer")
})
