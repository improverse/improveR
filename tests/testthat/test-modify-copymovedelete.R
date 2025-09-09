
Sys.setenv(TEST_NAME="modify-copymovedelete")

httptest::with_mock_dir("prepare-modify-copymovedelete",{
  test_that("createTestFolder", {
    Sys.setenv(IMPROVER_TEST_REPLAY="T")
    improveConnect()
    setEditable(T)
    expect_false(Sys.getenv("IMPROVER_TOKEN")=="")
    TEST_FOLDER <- emptyFolderSetup()
    assign(x = "TEST_FOLDER",value = TEST_FOLDER,envir = globalenv())
  })
})

# Helper function to ensure TEST_FOLDER exists when running tests individually
ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME="modify-copymovedelete")
  if (!exists("TEST_FOLDER") || is.null(TEST_FOLDER)) {
    improveR::setEditable(TRUE)
    TEST_FOLDER <- improveR:::emptyFolderSetup()
    assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
    return(TEST_FOLDER)
  }
  return(TEST_FOLDER)
}

FAKE_RES_ID <- "XXXXXXXXXXXXX"
FAKE_ENTITY_ID <- "wrongrepo:wrongID"
FAKE_LONG_ENTITY_ID <- "http://wrongURL:8843/?path=wrongrepo:wrongID"
FAKE_PATH <- paste0(TEST_FOLDER, "/FAKE")

httptest::with_mock_dir("modifyNonExisting", {
  test_that("modify non existing|ics1139", {
    TEST_FOLDER <- ensureTestFolder()
    expectedMessage <- "Source {id} does not exist, could not execute"

    Sys.setenv(improver.logfile = "improver.log")
    improveConnect()
    tf <- createFolder(TEST_FOLDER)

    id <- FAKE_RES_ID

    failed <- copy(id, FAKE_ENTITY_ID)
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message, glue::glue(expectedMessage))

    failed <- move(id, FAKE_ENTITY_ID)
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message, glue::glue(expectedMessage))

    failed <- delete(id)
    expect_false(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message, glue::glue(expectedMessage))
  })
})

httptest::with_mock_dir("modifyToNonExisting", {
  test_that("modify to non existing|ics1139", {
    TEST_FOLDER <- ensureTestFolder()
    expectedMessage <- "Target {id} does not exist, could not {func}"

    Sys.setenv(improver.logfile = "improver.log")
    improveConnect()
    createFolder(TEST_FOLDER)

    id <- FAKE_RES_ID
    func <- "copy"
    failed <- copy(TEST_FOLDER, id)
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message, glue::glue(expectedMessage))

    func <- "move"
    failed <- move(TEST_FOLDER, id)
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message, glue::glue(expectedMessage))
  })
})

httptest::with_mock_dir("invalidTargetNamesAndComments", {
  test_that("invalid target names and comments|ics1139", {
    TEST_FOLDER <- ensureTestFolder()
    expectedMessage <- "name needs to be of type character"

    Sys.setenv(improver.logfile = "improver.log")
    improveConnect()
    createFolder(TEST_FOLDER)

    id <- FAKE_RES_ID
    failed <- copy(TEST_FOLDER, id, targetName = 23)
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message, glue::glue(expectedMessage))

    failed <- move(TEST_FOLDER, id, targetName = data.frame())
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message, glue::glue(expectedMessage))

    expectedMessage <- "comment needs to be of type character"
    id <- FAKE_RES_ID
    failed <- copy(TEST_FOLDER, id, comment = 23)
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message, glue::glue(expectedMessage))

    failed <- move(TEST_FOLDER, id, comment = data.frame())
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message, glue::glue(expectedMessage))
  })
})

httptest::with_mock_dir("noOverwrite", {
  test_that("no overwrite|ics1139", {
    TEST_FOLDER <- ensureTestFolder()
    expectedMessage <- "{TEST_FOLDER}/tbOverwritten already exists, cannot {func}"

    Sys.setenv(improver.logfile = "improver.log")
    improveConnect()
    createFolder(TEST_FOLDER)

    sourceFile <- createFile(TEST_FOLDER, fileName = "overWriteFile")
    copyFile <- copy(sourceFile, target = TEST_FOLDER, targetName = "tbOverwritten")
    copyFile <- copy(sourceFile, target = TEST_FOLDER, targetName = "tbOverwritten", overwrite = T)
    expect_equal(copyFile$nodeType, "File")
    func <- "copy"
    failed <- copy(sourceFile, target = TEST_FOLDER, targetName = "tbOverwritten", overwrite = F)
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message, glue::glue(expectedMessage))
    func <- "move"
    failed <- move(sourceFile, target = TEST_FOLDER, targetName = "tbOverwritten", overwrite = F)
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message, glue::glue(expectedMessage))
    moveFile <- move(sourceFile, target = TEST_FOLDER, targetName = "tbOverwritten", overwrite = T)
    expect_equal(moveFile$nodeType, "File")

    sourceFile <- loadResource(sourceFile$path)
    expect_null(sourceFile)
    deleted <- delete(moveFile)
    expect_true(deleted)
    deleted <- delete(moveFile)
    expect_true(length(deleted) == 0 || !deleted)
  })
})

modifyToTarget <- function(s,t) {
  expectedMessage <- "{t$nodeType} is not an allowed target type for {s$nodeType}"

  failed <- copy(s,t)
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedMessage))

  failed <- move(s,t)
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedMessage))
}

httptest::with_mock_dir("modifyToWrongTarget", {
  test_that("modify to wrong target|ics1139", {
    TEST_FOLDER <- ensureTestFolder()
    Sys.setenv(improver.logfile = "improver.log")
    improveConnect()
    createFolder(TEST_FOLDER)

    # create resources in tree
    analysisTree <- createAnalysisTree(TEST_FOLDER, "errorCTree")
    expect_equal(analysisTree$nodeType, "Analysis Tree")
    expect_equal(analysisTree$name, "errorCTree")

    testFolder <- createFolder(TEST_FOLDER, "testCFolder")
    expect_equal(testFolder$nodeType, "Folder")
    expect_equal(testFolder$name, "testCFolder")

    testStep <- createStep(analysisTree, toolId = NULL)
    expect_equal(testStep$nodeType, "Step")
    expect_equal(testStep$name, "Step 1")

    testFile <- createFile(TEST_FOLDER, "testCFile")
    expect_equal(testFile$nodeType, "File")
    expect_equal(testFile$name, "testCFile")

    testLink <- createLink(TEST_FOLDER, testFile, "testCLink")
    expect_equal(testLink$nodeType, "Link")
    expect_equal(testLink$name, "testCLink")

    testExtLink <- createExternalLink(TEST_FOLDER, "testExtCLink", "http://scinteco.com")
    expect_equal(testExtLink$nodeType, "ExtLink")
    expect_equal(testExtLink$name, "testExtCLink")

    # analysis tree illegal targets
    modifyToTarget(analysisTree, analysisTree)
    modifyToTarget(analysisTree, testStep)
    modifyToTarget(analysisTree, testFile)
    modifyToTarget(analysisTree, testLink)
    modifyToTarget(analysisTree, testExtLink)

    # folder
    modifyToTarget(testFolder, testFile)
    modifyToTarget(testFolder, testLink)
    modifyToTarget(testFolder, testExtLink)

    # step
    modifyToTarget(testStep, testFolder)
    modifyToTarget(testStep, testStep)
    modifyToTarget(testStep, testFile)
    modifyToTarget(testStep, testLink)
    modifyToTarget(testStep, testExtLink)

    # file
    modifyToTarget(testFile, testFile)
    modifyToTarget(testFile, testLink)
    modifyToTarget(testFile, testExtLink)

    # link
    modifyToTarget(testLink, testFile)
    modifyToTarget(testLink, testLink)
    modifyToTarget(testLink, testExtLink)

    # extlink
    modifyToTarget(testExtLink, testFile)
    modifyToTarget(testExtLink, testLink)
    modifyToTarget(testExtLink, testExtLink)
  })
})

httptest::with_mock_dir("deleteLinksAfterDeletion", {
  test_that("delete links after deletion|ics1139", {
    Sys.setenv(improver.logfile = "improver.log")
    improveConnect()
    TEST_FOLDER <- ensureTestFolder()
    createFolder(TEST_FOLDER)

    # create file
    testFile <- createFile(TEST_FOLDER, "downloadFile")
    # download with and without idprefix
    withOutPrefix <- loadFile(testFile, linkInInventory = T)
    expect_true(file.exists(withOutPrefix$data[[1]]))

    withPrefix <- loadFile(testFile, addIdToName = T, linkInInventory = T)
    expect_true(file.exists(withPrefix$data[[1]]))
    expect_false(withPrefix$data[[1]] == withOutPrefix$data[[1]])

    unloadFile(withOutPrefix)
    expect_false(file.exists(withOutPrefix$data[[1]]))
    expect_true(file.exists(withPrefix$data[[1]]))

    unloadFile(withPrefix, addIdToName = T)
    expect_false(file.exists(withPrefix$data[[1]]))

    withOutPrefix <- loadFile(testFile, linkInInventory = T)
    expect_true(file.exists(withOutPrefix$data[[1]]))

    withPrefix <- loadFile(testFile, addIdToName = T, linkInInventory = T)
    expect_true(file.exists(withPrefix$data[[1]]))
    expect_false(withPrefix$data[[1]] == withOutPrefix$data[[1]])

    testResource <- loadResource(TEST_FOLDER)

    tryCatch({
      test <- loadFile(testResource)
    }, error = print)

    delete(TEST_FOLDER)

    expect_false(file.exists(withPrefix$data[[1]]))
    expect_false(file.exists(withOutPrefix$data[[1]]))
  })
})

tryCatch({
  file.rename(".improve.json","improve.json")
},error=print)

