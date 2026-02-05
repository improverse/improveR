
Sys.setenv(TEST_NAME="getMethodsErrors")

test_that("createTestFolder", {
  improveConnect()
  setEditable(T)
  expect_false(Sys.getenv("IMPROVER_TOKEN")=="")
  TEST_FOLDER <- baseFilesSetup()
  assign(x = "TEST_FOLDER",value = TEST_FOLDER,envir = globalenv())
})

# Helper function to ensure TEST_FOLDER exists when running tests individually
ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME="getMethodsErrors")
  if (!exists("TEST_FOLDER", envir = globalenv()) || is.null(get("TEST_FOLDER", envir = globalenv()))) {
    improveR::setEditable(TRUE)
    TEST_FOLDER <- improveR:::baseFilesSetup()
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

test_that("get non existant|ics1141", {
  TEST_FOLDER <- ensureTestFolder()
  FAKE_PATH <- getFakePath()
  failed <- getFile(FAKE_RES_ID)
  expect_null(failed)
  failed <- getCopy(FAKE_ENTITY_ID)
  expect_null(failed)
  failed <- getData(FAKE_LONG_ENTITY_ID)
  expect_null(failed)
  failed <- getFilesFromFolder(FAKE_RES_ID)
  expect_null(failed)
  failed <- getGraphics(FAKE_RES_ID)
  expect_null(failed)
  failed <- getHTML(FAKE_RES_ID)
  expect_null(failed)
  failed <- getMetaData(FAKE_RES_ID)
  expect_null(failed)
  failed <- getMetaDataMap(FAKE_RES_ID)
  expect_null(failed)
  failed <- getR(FAKE_RES_ID)
  expect_null(failed)
  failed <- getTextString(FAKE_RES_ID)
  expect_null(failed)
})


test_that("wrong type for get functions|ics1141", {
  TEST_FOLDER <- ensureTestFolder()
  Sys.setenv(improver.logfile="improver.log")
  improveConnect()
# test link resolving
  analysisTree <- createAnalysisTree(TEST_FOLDER,"errorCTree")
  expect_equal(analysisTree$nodeType,"Analysis Tree")
  expect_equal(analysisTree$name,"errorCTree")

  testFolder <- createFolder(TEST_FOLDER,"testCFolder")
  expect_equal(testFolder$nodeType,"Folder")
  expect_equal(testFolder$name,"testCFolder")


  testStep <- createStep(analysisTree,toolId = NULL)
  #test wrong tool id
  expect_equal(testStep$nodeType,"Step")
  expect_equal(testStep$name,"Step 1")

  testFile <- createFile(TEST_FOLDER,"testCFile")
  expect_equal(testFile$nodeType,"File")
  expect_equal(testFile$name,"testCFile")


  testLink <- createLink(TEST_FOLDER,testFile,"testCLink")
  expect_equal(testLink$nodeType,"Link")
  expect_equal(testLink$name,"testCLink")


  testExtLink <- createExternalLink(TEST_FOLDER,"testExtCLink","http://scinteco.com")
  expect_equal(testExtLink$nodeType,"ExtLink")
  expect_equal(testExtLink$name,"testExtCLink")

  expectedMessage <- "Get functions only available for links and files. {eId} is of type {typ}"

  failed <- getCopy(analysisTree)
  expect_null(failed)
  eId <- analysisTree$entityId
  typ <- analysisTree$nodeType
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedMessage))


  failed <- getCopy(testFolder)
  expect_null(failed)
  eId <- testFolder$entityId
  typ <- testFolder$nodeType
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedMessage))


  failed <- improveR::getCopy(testStep)
  expect_null(failed)
  eId <- testStep$entityId
  typ <- testStep$nodeType
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedMessage))


  failed <- improveR::getCopy(testExtLink)
  expect_null(failed)
  eId <- testExtLink$entityId
  typ <- testExtLink$nodeType
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedMessage))
#test loadfile mit link

  testFile <- updateFileContent(testFile,"improver.log")
  testLink2 <-createLink(TEST_FOLDER,testFile,"testCLink2")

  linkTest <- getCopy(testLink)
  linkTest2 <- getCopy(testLink2)
  expect_equal(file.size(linkTest$path),0)
  expect_gt(file.size(linkTest2$path),0)
  #targetEntityVersionId wrong

  # getFilesFromFolder wrong type
  expectedMessage <- "getFilesFromFolder ident must specify a container resource like Step, AnalysisTree or Folder {eId} specifies a {typ}"


  failed <- getFilesFromFolder(testExtLink)
  expect_null(failed)
  eId <- testExtLink$entityId
  typ <- testExtLink$nodeType
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedMessage))


  failed <- getFilesFromFolder(testLink)
  expect_null(failed)
  eId <- testLink$entityId
  typ <- testLink$nodeType
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedMessage))

  failed <- getFilesFromFolder(testFile)
  expect_null(failed)
  eId <- testFile$entityId
  typ <- testFile$nodeType
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedMessage))
})


test_that("multiple resources for getFiles from folder, illegal pattern|ics1141", {
  TEST_FOLDER <- ensureTestFolder()
  Sys.setenv(improver.logfile="improver.log")
  improveConnect()

  expectedMessage <- "getFilesFromFolder ident must specify exactly one resource {folder1$entityId}, {folder2$entityId}"

  folder1 <- createFolder(TEST_FOLDER,"folder1")
  expect_equal(folder1$nodeType,"Folder")
  folder2 <- createFolder(TEST_FOLDER,"folder2")
  expect_equal(folder1$nodeType,"Folder")

  folders <- rbind(folder1,folder2)
  failed <- getFilesFromFolder(folders)
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedMessage))

  # illegal pattern for getfilesfromfolder
  folderChildren <- getFilesFromFolder(TEST_FOLDER)
  expect_true(is.data.frame(folderChildren))
  folderChildren <- getFilesFromFolder(TEST_FOLDER,filePattern ="{{}")
#check failed check
})


