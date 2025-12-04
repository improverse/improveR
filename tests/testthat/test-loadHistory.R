Sys.setenv(TEST_NAME="history")

# Helper function to ensure TEST_FOLDER exists when running tests individually
ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME="history")
  improveR::setEditable(TRUE) #was moved outside of if clause; otherwise if test folder already exists, tests can't write.
  if (!exists("TEST_FOLDER") || is.null(TEST_FOLDER)) {
    TEST_FOLDER <- improveR:::workflowFilesSetup()
    assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
    return(TEST_FOLDER)
  }
  return(TEST_FOLDER)
}

test_that("loadParentalDescendants correctly identifies descendants", {
  TEST_FOLDER <- ensureTestFolder()
  Sys.getenv("TEST_NAME")
  Sys.getenv("TEST_FOLDER")

  #create 2 folder
  Folder1 <- improveR::createFolder(
    targetIdent = TEST_FOLDER,
    folderName = "Folder1"
  )
  Folder2 <- improveR::createFolder(
    targetIdent = TEST_FOLDER,
    folderName = "Folder2"
  )

  # Create trees
  tree1Folder1 <- createAnalysisTree(
    targetIdent = paste0(TEST_FOLDER, "/Folder1"),
    treeName = "tree1Folder1"
  )
  tree2Folder1 <- createAnalysisTree(
    targetIdent = paste0(TEST_FOLDER, "/Folder1"),
    treeName = "tree2Folder1"
  )
  tree1Folder2 <- createAnalysisTree(
    targetIdent = paste0(TEST_FOLDER, "/Folder2"),
    treeName = "tree1Folder2"
  )

  # Create steps
  step1Tree1Folder1 <- createStep(treeIdent = tree1Folder1) #parental location
  # Create file to be copied and checked for
  file <- createFile(targetIdent = step1Tree1Folder1, fileName = "file")

  # Descendants' location
  step2Tree1Folder1 <- createStep(treeIdent = tree1Folder1) # same folder, tree, different step
  step1Tree2Folder1 <- createStep(treeIdent = tree2Folder1) # same folder, differen tree, diff step
  step1Tree1Folder2 <- createStep(treeIdent = tree1Folder2) # diff folder, tree and step

  # Possible parental dscendant locations:
  # same folder - same tree - differen step
  copy(sources = file$resourceId, target = step2Tree1Folder1)
  # same folder - different tree - differen step
  copy(sources = file$resourceId, target = step1Tree2Folder1)
  # different folder - different tree - differen step
  copy(sources = file$resourceId, target = step1Tree1Folder2)

  #function result
  parentalDescendantHistory <- loadParentalDescendant(
    ident = file$resourceId
  )$data[[1]]$path %>%
    stringr::str_remove(., "[/\\\\]+$")    

  #collect descendants
  descendants <- c(
    step2Tree1Folder1$path,
    step1Tree2Folder1$path,
    step1Tree1Folder2$path
  )

 expect_equal(length(descendants),length(parentalDescendantHistory))
 expect_true(all(parentalDescendantHistory %in% descendants))
  
})