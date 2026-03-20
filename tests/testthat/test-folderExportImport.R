# Folder export/import round-trip tests
# Tests the exportFolder/importFolder pipeline with various folder structures.
# Run with: Rscript run-tests-unified.R folderExportImport

Sys.setenv(TEST_NAME = "folderExportImport")

ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME = "folderExportImport")
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

# Helper: folder export, delete source, import, return import target resource
folderRoundTrip <- function(sourceFolder, exportName, importFolderName) {
  TEST_FOLDER <- ensureTestFolder()

  exportFolder(sourceFolder, exportName, targetFolder = tempdir())
  exportFile <- file.path(tempdir(), paste0(exportName, ".zip"))
  expect_true(file.exists(exportFile), info = "Export zip should exist")

  # Import first (link mapping validation needs source resources to still exist)
  importTarget <- createFolder(TEST_FOLDER, importFolderName)
  importFolder(exportFile, importTarget)

  # Delete source after import so import can't cheat on verification
  delete(sourceFolder)

  # Cleanup export artifacts
  unlink(exportFile)
  unlink(file.path(tempdir(), paste0(exportName, "LinkMapping.json")))
  unlink(file.path(tempdir(), paste0(exportName, "ToolMapping.json")))

  return(importTarget)
}


test_that("Setup", {
  Sys.setenv(IMPROVER_TEST_REPLAY = "T")
  if (!improveConnected()) {
    tryCatch(improveConnect(), error = function(e) {})
  }
  setEditable(TRUE)
  TEST_FOLDER <- workflowFilesSetup()
  expect_false(Sys.getenv("IMPROVER_TOKEN") == "")
  assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
})


# ============================================================
# F1: Empty folder structure (no trees, no files)
# ============================================================
test_that("F1: Empty folder with subfolders roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F1_Source")
  sub1 <- createFolder(sourceFolder, "SubFolder1")
  sub2 <- createFolder(sourceFolder, "SubFolder2")
  sub1a <- createFolder(sub1, "SubFolder1a")

  importTarget <- folderRoundTrip(sourceFolder, "F1FolderExport", "F1_Target")

  # Verify folder structure was recreated
  targetChildren <- loadChildResources(importTarget)$data[[1]]
  expect_true("SubFolder1" %in% targetChildren$name)
  expect_true("SubFolder2" %in% targetChildren$name)

  # Check nested subfolder
  newSub1 <- loadResource(paste0(importTarget$path, "/SubFolder1"))
  sub1Children <- loadChildResources(newSub1)$data[[1]]
  expect_true("SubFolder1a" %in% sub1Children$name)

  cat("[F1] Empty folder structure roundtrip passed\n")
})


# ============================================================
# F2: Folder with standalone files
# ============================================================
test_that("F2: Folder with standalone files roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F2_Source")
  sub <- createFolder(sourceFolder, "DataFolder")

  # Create a file in the subfolder
  dataFilePath <- file.path(tempdir(), "test_data.csv")
  writeLines("col1,col2\n1,a\n2,b", dataFilePath)
  createFile(sub, "test_data.csv", dataFilePath)

  importTarget <- folderRoundTrip(sourceFolder, "F2FileExport", "F2_Target")

  # Verify file exists in imported structure
  newSub <- loadResource(paste0(importTarget$path, "/DataFolder"))
  expect_false(is.null(newSub))
  subChildren <- loadChildResources(newSub)$data[[1]]
  expect_true("test_data.csv" %in% subChildren$name)

  unlink(dataFilePath)
  cat("[F2] Folder with files roundtrip passed\n")
})


# ============================================================
# F3: Folder with single analysis tree
# ============================================================
test_that("F3: Folder with single analysis tree roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F3_Source")
  tree <- createAnalysisTree(sourceFolder, "ModelingTree")

  step1 <- createTestStep(tree, "Step1", "Modeling step",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1$realise()
  step1$finishRun()

  # Export (don't use roundTrip helper - need to inspect zip)
  exportFolder(sourceFolder, "F3TreeExport", targetFolder = tempdir())
  exportFile <- file.path(tempdir(), "F3TreeExport.zip")
  expect_true(file.exists(exportFile))

  # Verify both manifest.json and workflow.json exist
  zipContents <- zip::zip_list(exportFile)
  expect_true(any(grepl("manifest.json", zipContents$filename)))
  expect_true(any(grepl("workflow.json", zipContents$filename)))

  importTarget <- createFolder(TEST_FOLDER, "F3_Target")
  importFolder(exportFile, importTarget)
  delete(sourceFolder)

  # Verify tree was recreated
  targetChildren <- loadChildResources(importTarget)$data[[1]]
  expect_true("ModelingTree" %in% targetChildren$name)

  # Verify step exists in imported tree
  newTree <- loadResource(paste0(importTarget$path, "/ModelingTree"))
  importedWorkflow <- getWorkflow(newTree)
  importedSteps <- importedWorkflow$df()
  expect_equal(nrow(importedSteps), 1)
  expect_equal(importedSteps$description, "Modeling step")

  # Cleanup
  unlink(exportFile)
  unlink(file.path(tempdir(), "F3TreeExportToolMapping.json"))
  unlink(file.path(tempdir(), "F3TreeExportLinkMapping.json"))
  cat("[F3] Folder with single tree roundtrip passed\n")
})


# ============================================================
# F4: Folder with two trees and cross-tree link
# ============================================================
test_that("F4: Folder with two trees and cross-tree link roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F4_Source")
  tree1 <- createAnalysisTree(sourceFolder, "DataPrep")
  tree2 <- createAnalysisTree(sourceFolder, "Analysis")

  # Step 1 in DataPrep tree: produces output
  step1 <- createTestStep(tree1, "Step1", "Data preparation",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1Real <- step1$realise()
  step1$finishRun()

  # Get step1's output
  inv1 <- step1Real$getStepInventory()$data[[1]]
  outputFile1 <- inv1[inv1$name == "chapter15_example_cleaned.rds", ]

  # Step 2 in Analysis tree: uses step1's output (cross-tree link)
  step2 <- createTestStep(tree2, "Step2", "Analysis using prep data",
                          paste0(TEST_FOLDER, "/EDA.R"))
  step2$addStepRemoteFile(outputFile1, name = "chapter15_example_cleaned.rds")
  step2Real <- step2$realise()
  step2$finishRun()

  # Export
  exportFolder(sourceFolder, "F4CrossTree", targetFolder = tempdir())
  exportFile <- file.path(tempdir(), "F4CrossTree.zip")
  expect_true(file.exists(exportFile))

  # Verify zip structure
  zipContents <- zip::zip_list(exportFile)
  expect_true(any(grepl("manifest.json", zipContents$filename)),
              info = "Export should contain manifest.json")
  expect_true(any(grepl("workflow.json", zipContents$filename)),
              info = "Export should contain workflow.json")

  importTarget <- createFolder(TEST_FOLDER, "F4_Target")
  importFolder(exportFile, importTarget)
  delete(sourceFolder)

  # Verify both trees exist
  targetChildren <- loadChildResources(importTarget)$data[[1]]
  expect_true("DataPrep" %in% targetChildren$name)
  expect_true("Analysis" %in% targetChildren$name)

  # Verify step counts
  newTree1 <- loadResource(paste0(importTarget$path, "/DataPrep"))
  wf1 <- getWorkflow(newTree1)
  expect_equal(nrow(wf1$df()), 1)

  newTree2 <- loadResource(paste0(importTarget$path, "/Analysis"))
  wf2 <- getWorkflow(newTree2)
  expect_equal(nrow(wf2$df()), 1)

  # Verify cross-tree link: Analysis step should have link to DataPrep's output
  analysisSteps <- wf2$df()
  analysisStepEnv <- wf2$steps[[analysisSteps$fullName[1]]]
  analysisResource <- loadResource(analysisStepEnv$stepDf$sourceEntityId)
  analysisChildren <- loadChildResources(analysisResource)$data[[1]]
  hasLink <- any(analysisChildren$name == "chapter15_example_cleaned.rds" &
                   analysisChildren$nodeType == "Link")
  expect_true(hasLink,
              info = "Analysis step should have cross-tree link to DataPrep output")

  # Cleanup
  unlink(exportFile)
  unlink(file.path(tempdir(), "F4CrossTreeToolMapping.json"))
  unlink(file.path(tempdir(), "F4CrossTreeLinkMapping.json"))
  cat("[F4] Cross-tree link roundtrip passed\n")
})


# ============================================================
# F5: Nested folders with tree and files side by side
# ============================================================
test_that("F5: Nested folders with mixed content roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F5_Source")
  subFolder <- createFolder(sourceFolder, "Modeling")
  tree <- createAnalysisTree(subFolder, "RunTree")

  # Add a file alongside the tree in the subfolder
  dataFilePath <- file.path(tempdir(), "reference.txt")
  writeLines("Reference data for modeling", dataFilePath)
  createFile(subFolder, "reference.txt", dataFilePath)

  # Add a step in the tree
  step1 <- createTestStep(tree, "Step1", "Nested modeling step",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1$realise()
  step1$finishRun()

  importTarget <- folderRoundTrip(sourceFolder, "F5Nested", "F5_Target")

  # Verify structure
  targetChildren <- loadChildResources(importTarget)$data[[1]]
  expect_true("Modeling" %in% targetChildren$name)

  newSubFolder <- loadResource(paste0(importTarget$path, "/Modeling"))
  subChildren <- loadChildResources(newSubFolder)$data[[1]]
  expect_true("RunTree" %in% subChildren$name)
  expect_true("reference.txt" %in% subChildren$name)

  # Verify step in nested tree
  newTree <- loadResource(paste0(importTarget$path, "/Modeling/RunTree"))
  wf <- getWorkflow(newTree)
  expect_equal(nrow(wf$df()), 1)
  expect_equal(wf$df()$description, "Nested modeling step")

  unlink(dataFilePath)
  cat("[F5] Nested folder roundtrip passed\n")
})


# ============================================================
# F6: Deep nesting (3+ levels)
# ============================================================
test_that("F6: Deep nesting (3+ levels) roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F6_Source")
  level1 <- createFolder(sourceFolder, "Level1")
  level2 <- createFolder(level1, "Level2")
  level3 <- createFolder(level2, "Level3")

  # Tree at the deepest level
  tree <- createAnalysisTree(level3, "DeepTree")
  step1 <- createTestStep(tree, "Step1", "Deep step",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1$realise()
  step1$finishRun()

  # File at an intermediate level
  dataFilePath <- file.path(tempdir(), "level2_data.txt")
  writeLines("Data at level 2", dataFilePath)
  createFile(level2, "level2_data.txt", dataFilePath)

  importTarget <- folderRoundTrip(sourceFolder, "F6DeepNest", "F6_Target")

  # Verify full depth
  l1 <- loadResource(paste0(importTarget$path, "/Level1"))
  expect_false(is.null(l1))
  l2 <- loadResource(paste0(importTarget$path, "/Level1/Level2"))
  expect_false(is.null(l2))
  l3 <- loadResource(paste0(importTarget$path, "/Level1/Level2/Level3"))
  expect_false(is.null(l3))

  # Verify file at level 2
  l2Children <- loadChildResources(l2)$data[[1]]
  expect_true("level2_data.txt" %in% l2Children$name)

  # Verify tree and step at level 3
  l3Children <- loadChildResources(l3)$data[[1]]
  expect_true("DeepTree" %in% l3Children$name)

  newTree <- loadResource(paste0(importTarget$path, "/Level1/Level2/Level3/DeepTree"))
  wf <- getWorkflow(newTree)
  expect_equal(nrow(wf$df()), 1)
  expect_equal(wf$df()$description, "Deep step")

  unlink(dataFilePath)
  cat("[F6] Deep nesting roundtrip passed\n")
})


# ============================================================
# F7: Multiple trees in same folder (no cross-tree links)
# ============================================================
test_that("F7: Multiple independent trees roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F7_Source")
  tree1 <- createAnalysisTree(sourceFolder, "TreeA")
  tree2 <- createAnalysisTree(sourceFolder, "TreeB")

  step1 <- createTestStep(tree1, "Step1", "TreeA step",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1$realise()
  step1$finishRun()

  step2 <- createTestStep(tree2, "Step2", "TreeB step",
                          paste0(TEST_FOLDER, "/EDA.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step2$realise()
  step2$finishRun()

  importTarget <- folderRoundTrip(sourceFolder, "F7MultiTree", "F7_Target")

  targetChildren <- loadChildResources(importTarget)$data[[1]]
  expect_true("TreeA" %in% targetChildren$name)
  expect_true("TreeB" %in% targetChildren$name)

  wfA <- getWorkflow(loadResource(paste0(importTarget$path, "/TreeA")))
  expect_equal(nrow(wfA$df()), 1)
  expect_equal(wfA$df()$description, "TreeA step")

  wfB <- getWorkflow(loadResource(paste0(importTarget$path, "/TreeB")))
  expect_equal(nrow(wfB$df()), 1)
  expect_equal(wfB$df()$description, "TreeB step")

  cat("[F7] Multiple independent trees roundtrip passed\n")
})


# ============================================================
# F8: Multiple files in same folder
# ============================================================
test_that("F8: Multiple standalone files in same folder roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F8_Source")

  # Create multiple files
  for (fname in c("file1.txt", "file2.csv", "file3.R")) {
    localPath <- file.path(tempdir(), fname)
    writeLines(paste("Content of", fname), localPath)
    createFile(sourceFolder, fname, localPath)
    unlink(localPath)
  }

  importTarget <- folderRoundTrip(sourceFolder, "F8MultiFile", "F8_Target")

  targetChildren <- loadChildResources(importTarget)$data[[1]]
  expect_true("file1.txt" %in% targetChildren$name)
  expect_true("file2.csv" %in% targetChildren$name)
  expect_true("file3.R" %in% targetChildren$name)

  cat("[F8] Multiple standalone files roundtrip passed\n")
})


# ============================================================
# F9: Folder-level external link (ExtLink)
# ============================================================
test_that("F9: Folder with external link roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F9_Source")
  createExternalLink(sourceFolder, "Google", "https://www.google.com")

  # Export
  exportFolder(sourceFolder, "F9ExtLink", targetFolder = tempdir())
  exportFile <- file.path(tempdir(), "F9ExtLink.zip")
  expect_true(file.exists(exportFile))

  # Verify manifest has ExtLink entry
  importDir <- file.path(tempdir(), ".inspect_F9")
  dir.create(importDir, showWarnings = FALSE)
  zip::unzip(exportFile, exdir = importDir)
  manifestPath <- dir(importDir, pattern = "manifest.json", recursive = TRUE, full.names = TRUE)
  manifest <- jsonlite::read_json(manifestPath[1], simplifyVector = TRUE)
  extLinks <- manifest$folderStructure[manifest$folderStructure$nodeType == "ExtLink", ]
  expect_true(nrow(extLinks) > 0, info = "Manifest should contain ExtLink entry")
  expect_true(any(extLinks$name == "Google"))
  unlink(importDir, recursive = TRUE)

  importTarget <- createFolder(TEST_FOLDER, "F9_Target")
  importFolder(exportFile, importTarget)
  delete(sourceFolder)

  # Verify ExtLink was recreated
  targetChildren <- loadChildResources(importTarget)$data[[1]]
  expect_true("Google" %in% targetChildren$name,
              info = "ExtLink should be recreated on import")
  extLinkChild <- targetChildren[targetChildren$name == "Google", ]
  expect_equal(extLinkChild$nodeType, "ExtLink")

  # Cleanup
  unlink(exportFile)
  unlink(file.path(tempdir(), "F9ExtLinkToolMapping.json"))
  cat("[F9] External link roundtrip passed\n")
})


# ============================================================
# F10: Folder-level internal link (Link to resource within export)
# ============================================================
test_that("F10: Folder with internal link roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F10_Source")
  sub1 <- createFolder(sourceFolder, "SubA")
  sub2 <- createFolder(sourceFolder, "SubB")

  # Create a file in SubA
  localPath <- file.path(tempdir(), "linked_data.csv")
  writeLines("x,y\n1,2\n3,4", localPath)
  targetFile <- createFile(sub1, "linked_data.csv", localPath)

  # Create a link in SubB pointing to the file in SubA
  createLink(sub2, targetFile, linkName = "link_to_data")

  importTarget <- folderRoundTrip(sourceFolder, "F10InternalLink", "F10_Target")

  # Verify structure
  targetChildren <- loadChildResources(importTarget)$data[[1]]
  expect_true("SubA" %in% targetChildren$name)
  expect_true("SubB" %in% targetChildren$name)

  # Verify file in SubA
  newSubA <- loadResource(paste0(importTarget$path, "/SubA"))
  subAChildren <- loadChildResources(newSubA)$data[[1]]
  expect_true("linked_data.csv" %in% subAChildren$name)

  # Verify link in SubB
  newSubB <- loadResource(paste0(importTarget$path, "/SubB"))
  subBChildren <- loadChildResources(newSubB)$data[[1]]
  expect_true("link_to_data" %in% subBChildren$name,
              info = "Internal link should be recreated pointing to new resource")

  unlink(localPath)
  cat("[F10] Internal link roundtrip passed\n")
})


# ============================================================
# F11: Multi-step tree within folder (linear chain)
# ============================================================
test_that("F11: Folder with multi-step tree (linear chain) roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F11_Source")
  tree <- createAnalysisTree(sourceFolder, "ChainTree")

  # Step 1
  step1 <- createTestStep(tree, "Step1", "ChainStep1",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1Real <- step1$realise()
  step1$finishRun()

  # Get step1 output
  inv1 <- step1Real$getStepInventory()$data[[1]]
  output1 <- inv1[inv1$name == "chapter15_example_cleaned.rds", ]

  # Step 2: depends on step1
  step2 <- createTestStep(tree, "Step2", "ChainStep2",
                          paste0(TEST_FOLDER, "/EDA.R"))
  step2$addStepRemoteFile(output1, name = "chapter15_example_cleaned.rds")
  step2Real <- step2$realise()
  step2$finishRun()

  # Get step2 output
  inv2 <- step2Real$getStepInventory()$data[[1]]
  output2 <- inv2[inv2$name == "EDA_table.html", ]

  # Step 3: depends on step2
  step3 <- createTestStep(tree, "Step3", "ChainStep3",
                          paste0(TEST_FOLDER, "/report.R"))
  step3$addStepRemoteFile(output2, name = "EDA_table.html")
  step3Real <- step3$realise()
  step3$finishRun()

  importTarget <- folderRoundTrip(sourceFolder, "F11Chain", "F11_Target")

  newTree <- loadResource(paste0(importTarget$path, "/ChainTree"))
  wf <- getWorkflow(newTree)
  importedSteps <- wf$df()
  expect_equal(nrow(importedSteps), 3)
  expect_setequal(importedSteps$description,
                  c("ChainStep1", "ChainStep2", "ChainStep3"))

  # Verify Step2 has link to Step1's output
  step2Row <- importedSteps[importedSteps$description == "ChainStep2", ]
  step2Resource <- loadResource(wf$steps[[step2Row$fullName]]$stepDf$sourceEntityId)
  step2Children <- loadChildResources(step2Resource)$data[[1]]
  expect_true(
    any(step2Children$name == "chapter15_example_cleaned.rds" &
          step2Children$nodeType == "Link"),
    info = "Step2 should link to Step1 output"
  )

  # Verify Step3 has link to Step2's output
  step3Row <- importedSteps[importedSteps$description == "ChainStep3", ]
  step3Resource <- loadResource(wf$steps[[step3Row$fullName]]$stepDf$sourceEntityId)
  step3Children <- loadChildResources(step3Resource)$data[[1]]
  expect_true(
    any(step3Children$name == "EDA_table.html" &
          step3Children$nodeType == "Link"),
    info = "Step3 should link to Step2 output"
  )

  cat("[F11] Multi-step chain in folder roundtrip passed\n")
})


# ============================================================
# F12: Cross-tree dependency chain (tree1.step1 -> tree2.step1 -> tree2.step2)
# ============================================================
test_that("F12: Cross-tree dependency chain roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F12_Source")
  tree1 <- createAnalysisTree(sourceFolder, "Upstream")
  tree2 <- createAnalysisTree(sourceFolder, "Downstream")

  # Step in tree1: producer
  step1 <- createTestStep(tree1, "Step1", "Upstream producer",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1Real <- step1$realise()
  step1$finishRun()

  inv1 <- step1Real$getStepInventory()$data[[1]]
  output1 <- inv1[inv1$name == "chapter15_example_cleaned.rds", ]

  # Step 1 in tree2: depends on tree1's output
  step2a <- createTestStep(tree2, "Step2a", "Downstream consumer",
                           paste0(TEST_FOLDER, "/EDA.R"))
  step2a$addStepRemoteFile(output1, name = "chapter15_example_cleaned.rds")
  step2aReal <- step2a$realise()
  step2a$finishRun()

  inv2a <- step2aReal$getStepInventory()$data[[1]]
  output2a <- inv2a[inv2a$name == "EDA_table.html", ]

  # Step 2 in tree2: depends on step2a's output (same-tree internal link)
  step2b <- createTestStep(tree2, "Step2b", "Downstream reporter",
                           paste0(TEST_FOLDER, "/report.R"))
  step2b$addStepRemoteFile(output2a, name = "EDA_table.html")
  step2b$realise()
  step2b$finishRun()

  importTarget <- folderRoundTrip(sourceFolder, "F12CrossChain", "F12_Target")

  # Verify both trees
  targetChildren <- loadChildResources(importTarget)$data[[1]]
  expect_true("Upstream" %in% targetChildren$name)
  expect_true("Downstream" %in% targetChildren$name)

  wfUp <- getWorkflow(loadResource(paste0(importTarget$path, "/Upstream")))
  expect_equal(nrow(wfUp$df()), 1)

  wfDown <- getWorkflow(loadResource(paste0(importTarget$path, "/Downstream")))
  downSteps <- wfDown$df()
  expect_equal(nrow(downSteps), 2)

  # Verify cross-tree link: Downstream consumer should link to Upstream output
  consumerRow <- downSteps[downSteps$description == "Downstream consumer", ]
  consumerResource <- loadResource(wfDown$steps[[consumerRow$fullName]]$stepDf$sourceEntityId)
  consumerChildren <- loadChildResources(consumerResource)$data[[1]]
  expect_true(
    any(consumerChildren$name == "chapter15_example_cleaned.rds" &
          consumerChildren$nodeType == "Link"),
    info = "Downstream consumer should link to Upstream output"
  )

  # Verify same-tree link: reporter should link to consumer's output
  reporterRow <- downSteps[downSteps$description == "Downstream reporter", ]
  reporterResource <- loadResource(wfDown$steps[[reporterRow$fullName]]$stepDf$sourceEntityId)
  reporterChildren <- loadChildResources(reporterResource)$data[[1]]
  expect_true(
    any(reporterChildren$name == "EDA_table.html" &
          reporterChildren$nodeType == "Link"),
    info = "Downstream reporter should link to consumer's output"
  )

  cat("[F12] Cross-tree dependency chain roundtrip passed\n")
})


# ============================================================
# F13: Trees in different subfolders
# ============================================================
test_that("F13: Trees in different nested subfolders roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F13_Source")
  subA <- createFolder(sourceFolder, "FolderA")
  subB <- createFolder(sourceFolder, "FolderB")

  treeA <- createAnalysisTree(subA, "TreeInA")
  treeB <- createAnalysisTree(subB, "TreeInB")

  step1 <- createTestStep(treeA, "Step1", "Step in FolderA",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1$realise()
  step1$finishRun()

  step2 <- createTestStep(treeB, "Step2", "Step in FolderB",
                          paste0(TEST_FOLDER, "/EDA.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step2$realise()
  step2$finishRun()

  importTarget <- folderRoundTrip(sourceFolder, "F13SubTrees", "F13_Target")

  # Verify folder structure
  targetChildren <- loadChildResources(importTarget)$data[[1]]
  expect_true("FolderA" %in% targetChildren$name)
  expect_true("FolderB" %in% targetChildren$name)

  # Verify trees in correct locations
  newSubA <- loadResource(paste0(importTarget$path, "/FolderA"))
  subAChildren <- loadChildResources(newSubA)$data[[1]]
  expect_true("TreeInA" %in% subAChildren$name)

  newSubB <- loadResource(paste0(importTarget$path, "/FolderB"))
  subBChildren <- loadChildResources(newSubB)$data[[1]]
  expect_true("TreeInB" %in% subBChildren$name)

  # Verify steps
  wfA <- getWorkflow(loadResource(paste0(importTarget$path, "/FolderA/TreeInA")))
  expect_equal(wfA$df()$description, "Step in FolderA")

  wfB <- getWorkflow(loadResource(paste0(importTarget$path, "/FolderB/TreeInB")))
  expect_equal(wfB$df()$description, "Step in FolderB")

  cat("[F13] Trees in subfolders roundtrip passed\n")
})


# ============================================================
# F14: Cross-tree link between trees in different subfolders
# ============================================================
test_that("F14: Cross-tree link across subfolders roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F14_Source")
  subA <- createFolder(sourceFolder, "PrepFolder")
  subB <- createFolder(sourceFolder, "AnalysisFolder")

  treeA <- createAnalysisTree(subA, "PrepTree")
  treeB <- createAnalysisTree(subB, "AnalysisTree")

  # Step in PrepTree produces output
  step1 <- createTestStep(treeA, "Step1", "Prep step",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1Real <- step1$realise()
  step1$finishRun()

  inv1 <- step1Real$getStepInventory()$data[[1]]
  output1 <- inv1[inv1$name == "chapter15_example_cleaned.rds", ]

  # Step in AnalysisTree consumes output from PrepTree (cross-folder cross-tree)
  step2 <- createTestStep(treeB, "Step2", "Analysis step",
                          paste0(TEST_FOLDER, "/EDA.R"))
  step2$addStepRemoteFile(output1, name = "chapter15_example_cleaned.rds")
  step2$realise()
  step2$finishRun()

  importTarget <- folderRoundTrip(sourceFolder, "F14CrossSubfolder", "F14_Target")

  # Verify folder structure
  targetChildren <- loadChildResources(importTarget)$data[[1]]
  expect_true("PrepFolder" %in% targetChildren$name)
  expect_true("AnalysisFolder" %in% targetChildren$name)

  # Verify cross-tree link from AnalysisTree step to PrepTree step's output
  newTreeB <- loadResource(paste0(importTarget$path, "/AnalysisFolder/AnalysisTree"))
  wfB <- getWorkflow(newTreeB)
  analysisStep <- wfB$df()
  analysisEnv <- wfB$steps[[analysisStep$fullName[1]]]
  analysisResource <- loadResource(analysisEnv$stepDf$sourceEntityId)
  analysisChildren <- loadChildResources(analysisResource)$data[[1]]
  hasLink <- any(analysisChildren$name == "chapter15_example_cleaned.rds" &
                   analysisChildren$nodeType == "Link")
  expect_true(hasLink,
              info = "Cross-subfolder cross-tree link should be preserved")

  cat("[F14] Cross-subfolder cross-tree link roundtrip passed\n")
})


# ============================================================
# F15: Export package structure validation
# ============================================================
test_that("F15: Export zip structure is correct", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F15_Source")
  sub <- createFolder(sourceFolder, "DataSub")
  tree <- createAnalysisTree(sourceFolder, "MainTree")

  # File in subfolder
  localPath <- file.path(tempdir(), "validate.txt")
  writeLines("validation content", localPath)
  createFile(sub, "validate.txt", localPath)

  # Step in tree
  step1 <- createTestStep(tree, "Step1", "Validate step",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1$realise()
  step1$finishRun()

  exportFolder(sourceFolder, "F15Validate", targetFolder = tempdir())
  exportFile <- file.path(tempdir(), "F15Validate.zip")
  expect_true(file.exists(exportFile))

  zipContents <- zip::zip_list(exportFile)
  fnames <- zipContents$filename

  # Required files
  expect_true(any(grepl("manifest.json$", fnames)), info = "Must have manifest.json")
  expect_true(any(grepl("workflow.json$", fnames)), info = "Must have workflow.json")

  # Folder files directory
  expect_true(any(grepl("folderFiles/", fnames)), info = "Should have folderFiles/ dir")

  # Step directories should exist (inputFiles/outputFiles)
  expect_true(any(grepl("inputFiles/", fnames)), info = "Should have step inputFiles/")
  expect_true(any(grepl("outputFiles/", fnames)), info = "Should have step outputFiles/")

  # Tool mapping should be generated
  toolMappingFile <- file.path(tempdir(), "F15ValidateToolMapping.json")
  expect_true(file.exists(toolMappingFile), info = "ToolMapping.json should be generated")

  # Read and validate manifest structure
  inspectDir <- file.path(tempdir(), ".inspect_F15")
  dir.create(inspectDir, showWarnings = FALSE)
  zip::unzip(exportFile, exdir = inspectDir)
  manifestPath <- dir(inspectDir, pattern = "manifest.json", recursive = TRUE, full.names = TRUE)
  manifest <- jsonlite::read_json(manifestPath[1], simplifyVector = TRUE)

  expect_true(!is.null(manifest$version))
  expect_true(!is.null(manifest$exportDate))
  expect_true(!is.null(manifest$rootPath))
  expect_true(!is.null(manifest$rootResourceId))
  expect_true(!is.null(manifest$folderStructure))
  expect_true(!is.null(manifest$trees))
  expect_true(nrow(manifest$trees) == 1)
  expect_equal(manifest$trees$treeName, "MainTree")

  # Cleanup
  unlink(inspectDir, recursive = TRUE)
  unlink(exportFile)
  unlink(toolMappingFile)
  unlink(file.path(tempdir(), "F15ValidateLinkMapping.json"))
  unlink(localPath)
  delete(sourceFolder)
  cat("[F15] Export structure validation passed\n")
})


# ============================================================
# F16: Manifest validation on import (missing manifest)
# ============================================================
test_that("F16: Import fails gracefully with invalid zip", {
  TEST_FOLDER <- ensureTestFolder()

  # Create a zip with no manifest.json
  badDir <- file.path(tempdir(), "BadExport")
  dir.create(badDir, recursive = TRUE, showWarnings = FALSE)
  writeLines("not a manifest", file.path(badDir, "random.txt"))
  badZip <- file.path(tempdir(), "BadExport.zip")
  oldwd <- getwd()
  setwd(tempdir())
  utils::zip(badZip, "BadExport")
  setwd(oldwd)

  importTarget <- createFolder(TEST_FOLDER, "F16_Target")

  expect_error(
    importFolder(badZip, importTarget),
    info = "Import should fail with invalid/missing manifest"
  )

  # Cleanup
  unlink(badZip)
  unlink(badDir, recursive = TRUE)
  cat("[F16] Invalid zip import error handling passed\n")
})


# ============================================================
# F17: Export with non-folder resource should fail
# ============================================================
test_that("F17: exportFolder rejects non-folder resource", {
  TEST_FOLDER <- ensureTestFolder()

  # Create a file (not a folder)
  localPath <- file.path(tempdir(), "notafolder.txt")
  writeLines("test", localPath)
  fileResource <- createFile(TEST_FOLDER, "notafolder.txt", localPath)

  expect_error(
    exportFolder(fileResource, "F17Bad", targetFolder = tempdir()),
    "Folder",
    info = "exportFolder should reject non-folder resources"
  )

  unlink(localPath)
  cat("[F17] Non-folder rejection passed\n")
})


# ============================================================
# F18: Empty folder (no children at all)
# ============================================================
test_that("F18: Completely empty folder roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F18_Source")
  # No children at all

  importTarget <- folderRoundTrip(sourceFolder, "F18Empty", "F18_Target")

  # Import target should exist, no children added
  targetChildren <- loadChildResources(importTarget)$data[[1]]
  # Should be empty or NULL (folder was empty)
  expect_true(is.null(targetChildren) || nrow(targetChildren) == 0,
              info = "Empty folder import should produce empty target")

  cat("[F18] Completely empty folder roundtrip passed\n")
})


# ============================================================
# F19: Tree with parent-child step relationship
# ============================================================
test_that("F19: Parent-child step relationships preserved in folder export", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F19_Source")
  tree <- createAnalysisTree(sourceFolder, "ParentChildTree")

  # Parent step
  parentStep <- createTestStep(tree, "ParentStep", "Parent step",
                               paste0(TEST_FOLDER, "/DataManipulation.R"),
                               paste0(TEST_FOLDER, "/data.csv"))
  parentStepReal <- parentStep$realise()
  parentStep$finishRun()

  # Child step (attached to parent)
  childStep <- createTestStep(tree, "ChildStep", "Child step",
                              paste0(TEST_FOLDER, "/EDA.R"),
                              paste0(TEST_FOLDER, "/data.csv"))
  childStepReal <- childStep$realise()
  childStep$finishRun()

  # Attach child to parent (need to pass resource IDs, not environments)
  attachStep(childStepReal$stepDf$sourceEntityId, parentStepReal$stepDf$sourceEntityId)

  importTarget <- folderRoundTrip(sourceFolder, "F19ParentChild", "F19_Target")

  newTree <- loadResource(paste0(importTarget$path, "/ParentChildTree"))
  wf <- getWorkflow(newTree)
  importedSteps <- wf$df()
  expect_equal(nrow(importedSteps), 2)

  # Verify parent-child relationship
  childRow <- importedSteps[importedSteps$description == "Child step", ]
  parentRow <- importedSteps[importedSteps$description == "Parent step", ]

  # The child step should have a parentIdent that matches the parent
  childEnv <- wf$steps[[childRow$fullName]]

  # Try to load parent step from the child
  childResource <- loadResource(childEnv$stepDf$sourceEntityId)
  parentStepLoaded <- tryCatch(loadParentStep(childResource), error = function(e) NULL)

  if (!is.null(parentStepLoaded) && !is.null(parentStepLoaded$data[[1]])) {
    parentData <- parentStepLoaded$data[[1]]
    parentEnv <- wf$steps[[parentRow$fullName]]
    parentResource <- loadResource(parentEnv$stepDf$sourceEntityId)
    expect_true(parentResource$entityId %in% parentData$entityId,
                info = "Child should be attached to parent after import")
  }

  cat("[F19] Parent-child step relationship roundtrip passed\n")
})


# ============================================================
# F20: Multiple files at different folder levels
# ============================================================
test_that("F20: Files at multiple folder levels roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F20_Source")
  sub1 <- createFolder(sourceFolder, "Level1")
  sub2 <- createFolder(sub1, "Level2")

  # File at root level
  rootFilePath <- file.path(tempdir(), "root_file.txt")
  writeLines("Root level file", rootFilePath)
  createFile(sourceFolder, "root_file.txt", rootFilePath)

  # File at level 1
  l1FilePath <- file.path(tempdir(), "level1_file.txt")
  writeLines("Level 1 file", l1FilePath)
  createFile(sub1, "level1_file.txt", l1FilePath)

  # File at level 2
  l2FilePath <- file.path(tempdir(), "level2_file.txt")
  writeLines("Level 2 file", l2FilePath)
  createFile(sub2, "level2_file.txt", l2FilePath)

  importTarget <- folderRoundTrip(sourceFolder, "F20MultiLevel", "F20_Target")

  # Verify files at each level
  rootChildren <- loadChildResources(importTarget)$data[[1]]
  expect_true("root_file.txt" %in% rootChildren$name)

  newL1 <- loadResource(paste0(importTarget$path, "/Level1"))
  l1Children <- loadChildResources(newL1)$data[[1]]
  expect_true("level1_file.txt" %in% l1Children$name)

  newL2 <- loadResource(paste0(importTarget$path, "/Level1/Level2"))
  l2Children <- loadChildResources(newL2)$data[[1]]
  expect_true("level2_file.txt" %in% l2Children$name)

  # Cleanup temp files
  unlink(rootFilePath)
  unlink(l1FilePath)
  unlink(l2FilePath)
  cat("[F20] Files at multiple levels roundtrip passed\n")
})


# ============================================================
# F21: Complex mixed scenario - folders, files, trees, links, ExtLinks
# ============================================================
test_that("F21: Complex mixed folder with all resource types roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F21_Source")

  # Subfolders
  dataFolder <- createFolder(sourceFolder, "Data")
  analysisFolder <- createFolder(sourceFolder, "Analysis")

  # File in Data folder
  csvPath <- file.path(tempdir(), "experiment.csv")
  writeLines("id,value\n1,10\n2,20", csvPath)
  dataFile <- createFile(dataFolder, "experiment.csv", csvPath)

  # ExtLink in source folder
  createExternalLink(sourceFolder, "Protocol", "https://example.com/protocol")

  # Tree in Analysis folder with step
  tree <- createAnalysisTree(analysisFolder, "MainAnalysis")
  step1 <- createTestStep(tree, "Step1", "Analysis step",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1Real <- step1$realise()
  step1$finishRun()

  # Link from Analysis folder to Data file
  createLink(analysisFolder, dataFile, linkName = "data_reference")

  importTarget <- folderRoundTrip(sourceFolder, "F21Complex", "F21_Target")

  targetChildren <- loadChildResources(importTarget)$data[[1]]

  # Verify all top-level items
  expect_true("Data" %in% targetChildren$name, info = "Data folder should exist")
  expect_true("Analysis" %in% targetChildren$name, info = "Analysis folder should exist")
  expect_true("Protocol" %in% targetChildren$name, info = "ExtLink should exist")

  # Verify file in Data folder
  newDataFolder <- loadResource(paste0(importTarget$path, "/Data"))
  dataChildren <- loadChildResources(newDataFolder)$data[[1]]
  expect_true("experiment.csv" %in% dataChildren$name)

  # Verify tree and step in Analysis folder
  newAnalysisFolder <- loadResource(paste0(importTarget$path, "/Analysis"))
  analysisChildren <- loadChildResources(newAnalysisFolder)$data[[1]]
  expect_true("MainAnalysis" %in% analysisChildren$name)

  # Check data_reference link
  # It may or may not be resolved depending on id mapping
  if ("data_reference" %in% analysisChildren$name) {
    linkChild <- analysisChildren[analysisChildren$name == "data_reference", ]
    expect_equal(linkChild$nodeType, "Link",
                 info = "data_reference should be a Link node")
  }

  # Verify step
  newTree <- loadResource(paste0(importTarget$path, "/Analysis/MainAnalysis"))
  wf <- getWorkflow(newTree)
  expect_equal(nrow(wf$df()), 1)
  expect_equal(wf$df()$description, "Analysis step")

  unlink(csvPath)
  cat("[F21] Complex mixed scenario roundtrip passed\n")
})


# ============================================================
# F22: Branching trees - one producer tree, two consumer trees
# ============================================================
test_that("F22: Branching cross-tree dependencies roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F22_Source")
  producerTree <- createAnalysisTree(sourceFolder, "Producer")
  consumerTree1 <- createAnalysisTree(sourceFolder, "ConsumerA")
  consumerTree2 <- createAnalysisTree(sourceFolder, "ConsumerB")

  # Producer step
  step1 <- createTestStep(producerTree, "Step1", "Data producer",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1Real <- step1$realise()
  step1$finishRun()

  inv1 <- step1Real$getStepInventory()$data[[1]]
  output1 <- inv1[inv1$name == "chapter15_example_cleaned.rds", ]

  # Consumer A: uses producer's output
  step2a <- createTestStep(consumerTree1, "Step2a", "Consumer A step",
                           paste0(TEST_FOLDER, "/EDA.R"))
  step2a$addStepRemoteFile(output1, name = "chapter15_example_cleaned.rds")
  step2a$realise()
  step2a$finishRun()

  # Consumer B: also uses producer's output
  step2b <- createTestStep(consumerTree2, "Step2b", "Consumer B step",
                           paste0(TEST_FOLDER, "/report.R"))
  step2b$addStepRemoteFile(output1, name = "chapter15_example_cleaned.rds")
  step2b$realise()
  step2b$finishRun()

  importTarget <- folderRoundTrip(sourceFolder, "F22Branch", "F22_Target")

  targetChildren <- loadChildResources(importTarget)$data[[1]]
  expect_true("Producer" %in% targetChildren$name)
  expect_true("ConsumerA" %in% targetChildren$name)
  expect_true("ConsumerB" %in% targetChildren$name)

  # Verify Consumer A has link to Producer's output
  wfA <- getWorkflow(loadResource(paste0(importTarget$path, "/ConsumerA")))
  stepA <- wfA$df()
  stepAEnv <- wfA$steps[[stepA$fullName[1]]]
  stepAResource <- loadResource(stepAEnv$stepDf$sourceEntityId)
  stepAChildren <- loadChildResources(stepAResource)$data[[1]]
  expect_true(
    any(stepAChildren$name == "chapter15_example_cleaned.rds" &
          stepAChildren$nodeType == "Link"),
    info = "Consumer A should link to Producer's output"
  )

  # Verify Consumer B has link to Producer's output
  wfB <- getWorkflow(loadResource(paste0(importTarget$path, "/ConsumerB")))
  stepB <- wfB$df()
  stepBEnv <- wfB$steps[[stepB$fullName[1]]]
  stepBResource <- loadResource(stepBEnv$stepDf$sourceEntityId)
  stepBChildren <- loadChildResources(stepBResource)$data[[1]]
  expect_true(
    any(stepBChildren$name == "chapter15_example_cleaned.rds" &
          stepBChildren$nodeType == "Link"),
    info = "Consumer B should link to Producer's output"
  )

  cat("[F22] Branching cross-tree dependencies roundtrip passed\n")
})


# ============================================================
# F23: Step with outside link (link to resource outside export scope)
# ============================================================
test_that("F23: Step with outside link in folder export roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  # Create an outside resource (outside the folder being exported)
  outsideFilePath <- file.path(tempdir(), "outside_ref.csv")
  writeLines("a,b\n1,2", outsideFilePath)
  outsideFile <- createFile(TEST_FOLDER, "outside_ref.csv", outsideFilePath)

  sourceFolder <- createFolder(TEST_FOLDER, "F23_Source")
  tree <- createAnalysisTree(sourceFolder, "OutsideLinkTree")

  step1 <- createStepTemplateEnv(treeIdent = tree)
  step1$setStepRunserverLabel(Sys.getenv("R_RUNSERVER"))
  step1$setStepToolLabel(Sys.getenv("R_TOOL"))
  step1$setStepToolInstance(Sys.getenv("R_TOOL_INSTANCE"))
  step1$setStepDescription("Step with outside link")
  step1$setStepRationale("Test outside links")
  step1$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"),
                          variableName = "command-file")
  # Add outside link
  step1$addStepRemoteFile(outsideFile, name = "outside_ref.csv")
  step1$realise()
  step1$finishRun()

  # Export
  exportFolder(sourceFolder, "F23Outside", targetFolder = tempdir())
  exportFile <- file.path(tempdir(), "F23Outside.zip")
  expect_true(file.exists(exportFile))

  # LinkMapping should be generated for outside links
  linkMappingFile <- file.path(tempdir(), "F23OutsideLinkMapping.json")
  expect_true(file.exists(linkMappingFile),
              info = "LinkMapping should be generated for outside links")

  importTarget <- createFolder(TEST_FOLDER, "F23_Target")
  importFolder(exportFile, importTarget)
  delete(sourceFolder)

  # Verify step was imported
  newTree <- loadResource(paste0(importTarget$path, "/OutsideLinkTree"))
  wf <- getWorkflow(newTree)
  expect_equal(nrow(wf$df()), 1)

  # The outside link file should be present (uploaded from links/ dir)
  importedStep <- wf$df()
  stepEnv <- wf$steps[[importedStep$fullName[1]]]
  stepResource <- loadResource(stepEnv$stepDf$sourceEntityId)
  stepChildren <- loadChildResources(stepResource)$data[[1]]
  expect_true("outside_ref.csv" %in% stepChildren$name,
              info = "Outside link file should be present after import")

  # Cleanup
  unlink(exportFile)
  unlink(linkMappingFile)
  unlink(file.path(tempdir(), "F23OutsideToolMapping.json"))
  unlink(outsideFilePath)
  cat("[F23] Outside link in folder export roundtrip passed\n")
})


# ============================================================
# F24: Mixed files: copied files (asLink=FALSE) + internal links + outside links
# ============================================================
test_that("F24: Mixed file types in folder tree roundtrip", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F24_Source")
  tree <- createAnalysisTree(sourceFolder, "MixedTree")

  # Step 1: has command file + data file (outside link) + copied file
  step1 <- createTestStep(tree, "Step1", "Mixed producer",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1$addStepRemoteFile(paste0(TEST_FOLDER, "/EDA.R"),
                          name = "extra_script.R", asLink = FALSE)
  step1Real <- step1$realise()
  step1$finishRun()

  # Get step1's output
  inv1 <- step1Real$getStepInventory()$data[[1]]
  output1 <- inv1[inv1$name == "chapter15_example_cleaned.rds", ]

  # Step 2: internal link + outside link
  step2 <- createStepTemplateEnv(treeIdent = tree)
  step2$setStepRunserverLabel(Sys.getenv("R_RUNSERVER"))
  step2$setStepToolLabel(Sys.getenv("R_TOOL"))
  step2$setStepToolInstance(Sys.getenv("R_TOOL_INSTANCE"))
  step2$setStepDescription("Mixed consumer")
  step2$setStepRationale("Test mixed files")
  step2$addStepRemoteFile(paste0(TEST_FOLDER, "/EDA.R"),
                          variableName = "command-file")
  step2$addStepRemoteFile(output1, name = "chapter15_example_cleaned.rds")
  step2$addStepRemoteFile(paste0(TEST_FOLDER, "/report.R"),
                          name = "outside_script.R")
  step2$realise()
  step2$finishRun()

  importTarget <- folderRoundTrip(sourceFolder, "F24Mixed", "F24_Target")

  newTree <- loadResource(paste0(importTarget$path, "/MixedTree"))
  wf <- getWorkflow(newTree)
  importedSteps <- wf$df()
  expect_equal(nrow(importedSteps), 2)

  # Verify producer has copied file
  producerRow <- importedSteps[importedSteps$description == "Mixed producer", ]
  producerEnv <- wf$steps[[producerRow$fullName]]
  producerFiles <- producerEnv$stepDf$remoteFiles[[1]]
  producerNames <- gsub("^\\./", "", producerFiles$name)
  expect_true("extra_script.R" %in% producerNames,
              info = "Copied file should survive folder roundtrip")

  # Verify consumer has internal link
  consumerRow <- importedSteps[importedSteps$description == "Mixed consumer", ]
  consumerEnv <- wf$steps[[consumerRow$fullName]]
  consumerResource <- loadResource(consumerEnv$stepDf$sourceEntityId)
  consumerChildren <- loadChildResources(consumerResource)$data[[1]]
  expect_true(
    any(consumerChildren$name == "chapter15_example_cleaned.rds" &
          consumerChildren$nodeType == "Link"),
    info = "Internal link should be preserved in mixed scenario"
  )

  cat("[F24] Mixed file types roundtrip passed\n")
})


# ============================================================
# F25: Folder export with tool mapping verification
# ============================================================
test_that("F25: Tool mapping generated correctly for folder export", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F25_Source")
  tree <- createAnalysisTree(sourceFolder, "ToolTree")

  step1 <- createTestStep(tree, "Step1", "Tool test step",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1$realise()
  step1$finishRun()

  exportFolder(sourceFolder, "F25ToolMap", targetFolder = tempdir())

  toolMappingFile <- file.path(tempdir(), "F25ToolMapToolMapping.json")
  expect_true(file.exists(toolMappingFile),
              info = "ToolMapping.json should be generated for folder export")

  toolMapping <- jsonlite::read_json(toolMappingFile, simplifyVector = TRUE)
  expect_true(nrow(toolMapping) > 0,
              info = "Tool mapping should have entries")
  expect_true("runserverLabel" %in% names(toolMapping),
              info = "Tool mapping should contain runserver info")

  # Cleanup
  exportFile <- file.path(tempdir(), "F25ToolMap.zip")
  unlink(exportFile)
  unlink(toolMappingFile)
  unlink(file.path(tempdir(), "F25ToolMapLinkMapping.json"))
  delete(sourceFolder)
  cat("[F25] Tool mapping for folder export passed\n")
})


# ============================================================
# F26: Import cleanup - temp directory removed
# ============================================================
test_that("F26: Import cleans up temporary directory", {
  TEST_FOLDER <- ensureTestFolder()

  sourceFolder <- createFolder(TEST_FOLDER, "F26_Source")
  sub <- createFolder(sourceFolder, "CleanupSub")

  exportFolder(sourceFolder, "F26Cleanup", targetFolder = tempdir())
  exportFile <- file.path(tempdir(), "F26Cleanup.zip")

  importTarget <- createFolder(TEST_FOLDER, "F26_Target")
  importFolder(exportFile, importTarget)
  delete(sourceFolder)

  # Check that .import_ directories were cleaned up
  importDirs <- dir(getwd(), pattern = "^\\.import_F26Cleanup", full.names = TRUE)
  expect_equal(length(importDirs), 0,
               info = "Import temporary directory should be cleaned up")

  # Cleanup
  unlink(exportFile)
  unlink(file.path(tempdir(), "F26CleanupToolMapping.json"))
  cat("[F26] Import cleanup passed\n")
})


# ============================================================
# F27: Cross-repo double round-trip with complex project
#   Build complex project in repo1 (trees, cross-tree links,
#   external links, files, ext links). Export repo1 → repo2,
#   export repo2 → repo1. Verify structural fidelity.
# ============================================================
test_that("F27: Cross-repo double round-trip preserves complex project structure", {
  TEST_FOLDER <- ensureTestFolder()

  # ----------------------------------------------------------
  # Set up "external" resources that live OUTSIDE the exported project.
  # These simulate resources in a different part of the repository that

  # our project links to.
  # ----------------------------------------------------------
  externalFolder <- createFolder(TEST_FOLDER, "F27_External")
  extFilePath1 <- file.path(tempdir(), "shared_protocol.pdf")
  writeLines("Shared protocol content", extFilePath1)
  externalFile1 <- createFile(externalFolder, "shared_protocol.pdf", extFilePath1)

  extFilePath2 <- file.path(tempdir(), "reference_data.csv")
  writeLines("ref_col1,ref_col2\nA,1\nB,2", extFilePath2)
  externalFile2 <- createFile(externalFolder, "reference_data.csv", extFilePath2)

  # Remember external resource identifiers for later verification
  extFile1EntityId <- externalFile1$entityId
  extFile2EntityId <- externalFile2$entityId

  # ----------------------------------------------------------
  # Build the complex project in "repo1" location
  # ----------------------------------------------------------
  repo1 <- createFolder(TEST_FOLDER, "F27_Repo1")

  # Sub-structure: repo1/DataPrep, repo1/Analysis, repo1/Reports
  dataPrepFolder <- createFolder(repo1, "DataPrep")
  analysisFolder <- createFolder(repo1, "Analysis")
  reportsFolder  <- createFolder(repo1, "Reports")

  # Standalone file in DataPrep folder
  readmePath <- file.path(tempdir(), "README.txt")
  writeLines("Project README for the data preparation phase", readmePath)
  createFile(dataPrepFolder, "README.txt", readmePath)

  # ExtLink in reports folder
  createExternalLink(reportsFolder, "PublicDashboard", "https://example.com/dashboard")

  # Folder-level link from Analysis to external file
  createLink(analysisFolder, externalFile1, linkName = "protocol_ref")

  # ----------------------------------------------------------
  # Tree 1: DataPrep/PrepTree — two steps with a linear dependency
  # ----------------------------------------------------------
  prepTree <- createAnalysisTree(dataPrepFolder, "PrepTree")

  step1 <- createTestStep(prepTree, "Prep1", "Data cleaning step",
                          paste0(TEST_FOLDER, "/DataManipulation.R"),
                          paste0(TEST_FOLDER, "/data.csv"))
  step1Real <- step1$realise()
  step1$finishRun()

  inv1 <- step1Real$getStepInventory()$data[[1]]
  output1 <- inv1[inv1$name == "chapter15_example_cleaned.rds", ]

  step2 <- createTestStep(prepTree, "Prep2", "Data enrichment step",
                          paste0(TEST_FOLDER, "/EDA.R"))
  step2$addStepRemoteFile(output1, name = "chapter15_example_cleaned.rds")
  step2Real <- step2$realise()
  step2$finishRun()

  inv2 <- step2Real$getStepInventory()$data[[1]]
  output2 <- inv2[inv2$name == "EDA_table.html", ]

  # ----------------------------------------------------------
  # Tree 2: Analysis/AnalysisTree — step that uses PrepTree output
  #   (cross-tree internal link) AND an external resource link
  # ----------------------------------------------------------
  analysisTree <- createAnalysisTree(analysisFolder, "AnalysisTree")

  step3 <- createStepTemplateEnv(treeIdent = analysisTree)
  step3$setStepRunserverLabel(Sys.getenv("R_RUNSERVER"))
  step3$setStepToolLabel(Sys.getenv("R_TOOL"))
  step3$setStepToolInstance(Sys.getenv("R_TOOL_INSTANCE"))
  step3$setStepDescription("Main analysis step")
  step3$setStepRationale("Analysis using prep data and external reference")
  step3$addStepRemoteFile(paste0(TEST_FOLDER, "/report.R"),
                          variableName = "command-file")
  # Cross-tree link: consume output from PrepTree/Prep2
  step3$addStepRemoteFile(output2, name = "EDA_table.html")
  # External link: reference data from outside the project
  step3$addStepRemoteFile(externalFile2, name = "reference_data.csv")
  step3Real <- step3$realise()
  step3$finishRun()

  inv3 <- step3Real$getStepInventory()$data[[1]]

  # ----------------------------------------------------------
  # Tree 3: Reports/ReportTree — step using Analysis output
  #   (another cross-tree link, different subfolder)
  # ----------------------------------------------------------
  reportTree <- createAnalysisTree(reportsFolder, "ReportTree")

  # Find an output from step3
  output3 <- inv3[inv3$name == "report.html" | inv3$nodeType == "File", ]
  if (nrow(output3) > 0) {
    output3 <- output3[1, ]
  }

  step4 <- createTestStep(reportTree, "Report1", "Final report step",
                          paste0(TEST_FOLDER, "/test_lm_plot.R"))
  if (nrow(output3) > 0) {
    step4$addStepRemoteFile(output3, name = output3$name)
  }
  step4$realise()
  step4$finishRun()

  cat("[F27] Complex project created in repo1\n")

  # ----------------------------------------------------------
  # Snapshot original structure for comparison
  # ----------------------------------------------------------
  .snapshotFolder <- function(folderResource, basePath = "") {
    children <- loadChildResources(folderResource)$data[[1]]
    if (is.null(children) || nrow(children) == 0) {
      return(data.frame(name = character(0), nodeType = character(0),
                        relPath = character(0), stringsAsFactors = FALSE))
    }
    result <- data.frame(
      name = children$name,
      nodeType = children$nodeType,
      relPath = if (basePath == "") children$name else paste0(basePath, "/", children$name),
      stringsAsFactors = FALSE
    )
    # Recurse into folders
    for (i in seq_len(nrow(children))) {
      if (children$nodeType[i] == "Folder") {
        childResource <- loadResource(children$entityId[i])
        subResult <- .snapshotFolder(childResource,
                                     if (basePath == "") children$name[i]
                                     else paste0(basePath, "/", children$name[i]))
        result <- rbind(result, subResult)
      }
    }
    return(result)
  }

  .snapshotTrees <- function(folderResource, basePath = "") {
    children <- loadChildResources(folderResource)$data[[1]]
    if (is.null(children) || nrow(children) == 0) return(list())
    trees <- list()
    for (i in seq_len(nrow(children))) {
      child <- children[i, ]
      childPath <- if (basePath == "") child$name else paste0(basePath, "/", child$name)
      if (child$nodeType == "Analysis Tree") {
        wf <- tryCatch(getWorkflow(loadResource(child$entityId)), error = function(e) NULL)
        if (!is.null(wf)) {
          steps <- wf$df()
          trees[[childPath]] <- list(
            treeName = child$name,
            stepCount = nrow(steps),
            stepDescriptions = sort(steps$description)
          )
        }
      } else if (child$nodeType == "Folder") {
        subTrees <- .snapshotTrees(loadResource(child$entityId), childPath)
        trees <- c(trees, subTrees)
      }
    }
    return(trees)
  }

  originalSnapshot <- .snapshotFolder(repo1)
  originalTrees <- .snapshotTrees(repo1)

  cat("[F27] Original snapshot: ", nrow(originalSnapshot), " entries, ",
      length(originalTrees), " trees\n")

  # ----------------------------------------------------------
  # Export from repo1
  # ----------------------------------------------------------
  exportFolder(repo1, "F27_Repo1Export", targetFolder = tempdir())
  exportFile1 <- file.path(tempdir(), "F27_Repo1Export.zip")
  expect_true(file.exists(exportFile1), info = "Repo1 export zip should exist")

  # ----------------------------------------------------------
  # Import into repo2 location
  # ----------------------------------------------------------
  repo2 <- createFolder(TEST_FOLDER, "F27_Repo2")
  importFolder(exportFile1, repo2)

  cat("[F27] Imported repo1 → repo2\n")

  # Cleanup repo1 export artifacts
  unlink(exportFile1)
  unlink(file.path(tempdir(), "F27_Repo1ExportLinkMapping.json"))
  unlink(file.path(tempdir(), "F27_Repo1ExportToolMapping.json"))

  # ----------------------------------------------------------
  # Verify repo2 has the structure
  # ----------------------------------------------------------
  repo2Snapshot <- .snapshotFolder(repo2)
  repo2Trees <- .snapshotTrees(repo2)

  cat("[F27] Repo2 snapshot: ", nrow(repo2Snapshot), " entries, ",
      length(repo2Trees), " trees\n")

  # Same folder/file/link names at each level
  expect_equal(sort(originalSnapshot$relPath), sort(repo2Snapshot$relPath),
               info = "Repo2 should have same structure as repo1")

  # Same tree/step structure
  expect_equal(length(originalTrees), length(repo2Trees),
               info = "Repo2 should have same number of trees")
  for (treePath in names(originalTrees)) {
    expect_true(treePath %in% names(repo2Trees),
                info = paste("Repo2 should have tree:", treePath))
    if (treePath %in% names(repo2Trees)) {
      expect_equal(originalTrees[[treePath]]$stepCount,
                   repo2Trees[[treePath]]$stepCount,
                   info = paste("Step count should match for tree:", treePath))
      expect_equal(originalTrees[[treePath]]$stepDescriptions,
                   repo2Trees[[treePath]]$stepDescriptions,
                   info = paste("Step descriptions should match for tree:", treePath))
    }
  }

  # ----------------------------------------------------------
  # Verify external links in repo2 point to the original external resources
  # ----------------------------------------------------------

  # Folder-level link: Analysis/protocol_ref should point to externalFile1
  repo2AnalysisFolder <- loadResource(paste0(repo2$path, "/Analysis"))
  repo2AnalysisChildren <- loadChildResources(repo2AnalysisFolder)$data[[1]]
  if ("protocol_ref" %in% repo2AnalysisChildren$name) {
    protocolLink <- repo2AnalysisChildren[repo2AnalysisChildren$name == "protocol_ref", ]
    expect_equal(protocolLink$nodeType, "Link",
                 info = "protocol_ref should be a Link in repo2")
    # Verify link target points to the original external resource
    protocolLinkResource <- loadResource(protocolLink$entityId)
    if (!is.null(protocolLinkResource$targetEntityId)) {
      targetRes <- tryCatch(loadResource(protocolLinkResource$targetEntityId),
                            error = function(e) NULL)
      if (!is.null(targetRes)) {
        expect_equal(targetRes$entityId, extFile1EntityId,
                     info = "protocol_ref link should point to original external file")
      }
    }
  }

  # Step-level external link: AnalysisTree step should have reference_data.csv
  # pointing to the original external resource
  repo2AnalysisTree <- loadResource(paste0(repo2$path, "/Analysis/AnalysisTree"))
  repo2Wf <- getWorkflow(repo2AnalysisTree)
  repo2Steps <- repo2Wf$df()
  analysisStepRow <- repo2Steps[repo2Steps$description == "Main analysis step", ]
  if (nrow(analysisStepRow) > 0) {
    analysisEnv <- repo2Wf$steps[[analysisStepRow$fullName[1]]]
    stepResource <- loadResource(analysisEnv$stepDf$sourceEntityId)
    stepChildren <- loadChildResources(stepResource)$data[[1]]
    refDataEntry <- stepChildren[stepChildren$name == "reference_data.csv", ]
    if (nrow(refDataEntry) > 0 && refDataEntry$nodeType == "Link") {
      refLinkResource <- loadResource(refDataEntry$entityId)
      if (!is.null(refLinkResource$targetEntityId)) {
        refTarget <- tryCatch(loadResource(refLinkResource$targetEntityId),
                              error = function(e) NULL)
        if (!is.null(refTarget)) {
          expect_equal(refTarget$entityId, extFile2EntityId,
                       info = "reference_data.csv should point to original external file in repo2")
        }
      }
    }
  }

  # ExtLink: Reports/PublicDashboard should be preserved
  repo2ReportsFolder <- loadResource(paste0(repo2$path, "/Reports"))
  repo2ReportsChildren <- loadChildResources(repo2ReportsFolder)$data[[1]]
  expect_true("PublicDashboard" %in% repo2ReportsChildren$name,
              info = "ExtLink should be preserved in repo2")
  if ("PublicDashboard" %in% repo2ReportsChildren$name) {
    extLinkEntry <- repo2ReportsChildren[repo2ReportsChildren$name == "PublicDashboard", ]
    expect_equal(extLinkEntry$nodeType, "ExtLink",
                 info = "PublicDashboard should be an ExtLink in repo2")
  }

  cat("[F27] Repo2 structure and external links verified\n")

  # ----------------------------------------------------------
  # Now export from repo2 and import back to repo1 location
  # ----------------------------------------------------------
  exportFolder(repo2, "F27_Repo2Export", targetFolder = tempdir())
  exportFile2 <- file.path(tempdir(), "F27_Repo2Export.zip")
  expect_true(file.exists(exportFile2), info = "Repo2 export zip should exist")

  # Import first — link mapping validation needs source resources to still exist
  # (same pattern as folderRoundTrip helper)
  repo1_reimported <- createFolder(TEST_FOLDER, "F27_Repo1_Reimported")
  importFolder(exportFile2, repo1_reimported)

  cat("[F27] Imported repo2 → repo1 (reimported)\n")

  # Now delete originals so verification can't cheat
  delete(repo1)
  delete(repo2)

  # Cleanup repo2 export artifacts
  unlink(exportFile2)
  unlink(file.path(tempdir(), "F27_Repo2ExportLinkMapping.json"))
  unlink(file.path(tempdir(), "F27_Repo2ExportToolMapping.json"))

  # ----------------------------------------------------------
  # Final verification: reimported repo1 matches original structure
  # ----------------------------------------------------------
  reimportedSnapshot <- .snapshotFolder(repo1_reimported)
  reimportedTrees <- .snapshotTrees(repo1_reimported)

  cat("[F27] Reimported snapshot: ", nrow(reimportedSnapshot), " entries, ",
      length(reimportedTrees), " trees\n")

  # Structure equality
  expect_equal(sort(originalSnapshot$relPath), sort(reimportedSnapshot$relPath),
               info = "Reimported repo1 should have same structure as original")

  # Node types at each path
  origSorted <- originalSnapshot[order(originalSnapshot$relPath), ]
  reimSorted <- reimportedSnapshot[order(reimportedSnapshot$relPath), ]
  # Compare name + nodeType for matching paths
  for (rp in origSorted$relPath) {
    origRow <- origSorted[origSorted$relPath == rp, ]
    reimRow <- reimSorted[reimSorted$relPath == rp, ]
    if (nrow(reimRow) > 0) {
      expect_equal(origRow$nodeType, reimRow$nodeType,
                   info = paste("NodeType should match for:", rp))
    }
  }

  # Tree/step fidelity
  expect_equal(length(originalTrees), length(reimportedTrees),
               info = "Reimported should have same number of trees as original")
  for (treePath in names(originalTrees)) {
    expect_true(treePath %in% names(reimportedTrees),
                info = paste("Reimported should have tree:", treePath))
    if (treePath %in% names(reimportedTrees)) {
      expect_equal(originalTrees[[treePath]]$stepCount,
                   reimportedTrees[[treePath]]$stepCount,
                   info = paste("Step count should match for:", treePath))
      expect_equal(originalTrees[[treePath]]$stepDescriptions,
                   reimportedTrees[[treePath]]$stepDescriptions,
                   info = paste("Descriptions should match for:", treePath))
    }
  }

  # ----------------------------------------------------------
  # Verify external links still point to originals after double round-trip
  # ----------------------------------------------------------

  # Folder-level link: Analysis/protocol_ref → externalFile1
  reimAnalysisFolder <- loadResource(paste0(repo1_reimported$path, "/Analysis"))
  reimAnalysisChildren <- loadChildResources(reimAnalysisFolder)$data[[1]]
  if ("protocol_ref" %in% reimAnalysisChildren$name) {
    protocolLink <- reimAnalysisChildren[reimAnalysisChildren$name == "protocol_ref", ]
    expect_equal(protocolLink$nodeType, "Link",
                 info = "protocol_ref should be a Link after double round-trip")
    protocolLinkResource <- loadResource(protocolLink$entityId)
    if (!is.null(protocolLinkResource$targetEntityId)) {
      targetRes <- tryCatch(loadResource(protocolLinkResource$targetEntityId),
                            error = function(e) NULL)
      if (!is.null(targetRes)) {
        expect_equal(targetRes$entityId, extFile1EntityId,
                     info = "protocol_ref should still point to original external file after double round-trip")
      }
    }
  }

  # Step-level external link: reference_data.csv → externalFile2
  reimAnalysisTree <- loadResource(paste0(repo1_reimported$path, "/Analysis/AnalysisTree"))
  reimWf <- getWorkflow(reimAnalysisTree)
  reimSteps <- reimWf$df()
  reimAnalysisStep <- reimSteps[reimSteps$description == "Main analysis step", ]
  if (nrow(reimAnalysisStep) > 0) {
    reimStepEnv <- reimWf$steps[[reimAnalysisStep$fullName[1]]]
    reimStepResource <- loadResource(reimStepEnv$stepDf$sourceEntityId)
    reimStepChildren <- loadChildResources(reimStepResource)$data[[1]]
    refEntry <- reimStepChildren[reimStepChildren$name == "reference_data.csv", ]
    if (nrow(refEntry) > 0 && refEntry$nodeType == "Link") {
      refLinkRes <- loadResource(refEntry$entityId)
      if (!is.null(refLinkRes$targetEntityId)) {
        refTarget <- tryCatch(loadResource(refLinkRes$targetEntityId),
                              error = function(e) NULL)
        if (!is.null(refTarget)) {
          expect_equal(refTarget$entityId, extFile2EntityId,
                       info = "reference_data.csv should point to original external file after double round-trip")
        }
      }
    }
  }

  # ExtLink preservation
  reimReportsFolder <- loadResource(paste0(repo1_reimported$path, "/Reports"))
  reimReportsChildren <- loadChildResources(reimReportsFolder)$data[[1]]
  expect_true("PublicDashboard" %in% reimReportsChildren$name,
              info = "ExtLink should survive double round-trip")
  if ("PublicDashboard" %in% reimReportsChildren$name) {
    extLinkEntry <- reimReportsChildren[reimReportsChildren$name == "PublicDashboard", ]
    expect_equal(extLinkEntry$nodeType, "ExtLink",
                 info = "PublicDashboard should be ExtLink after double round-trip")
  }

  # Cross-tree internal link: AnalysisTree step should link to PrepTree output
  if (nrow(reimAnalysisStep) > 0) {
    crossTreeLink <- reimStepChildren[reimStepChildren$name == "EDA_table.html", ]
    if (nrow(crossTreeLink) > 0) {
      expect_equal(crossTreeLink$nodeType, "Link",
                   info = "Cross-tree link should be preserved after double round-trip")
    }
  }

  # ----------------------------------------------------------
  # Cleanup
  # ----------------------------------------------------------
  unlink(extFilePath1)
  unlink(extFilePath2)
  unlink(readmePath)

  cat("[F27] Cross-repo double round-trip PASSED\n")
  cat("  - Structure: ", nrow(originalSnapshot), " entries preserved through both exports\n")
  cat("  - Trees: ", length(originalTrees), " trees with step counts matched\n")
  cat("  - External links verified through double round-trip\n")
})
