

Sys.setenv(TEST_NAME="errorMessages")

test_that("createTestFolder", {
  improveConnect()
  setEditable(T)
  expect_false(Sys.getenv("IMPROVER_TOKEN")=="")
  TEST_FOLDER <- improveR:::emptyFolderSetup()
  assign(x = "TEST_FOLDER",value = TEST_FOLDER,envir = globalenv())
})

# Helper function to ensure TEST_FOLDER exists when running tests individually
ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME="errorMessages")
  if (!exists("TEST_FOLDER", envir = globalenv()) || is.null(get("TEST_FOLDER", envir = globalenv()))) {
    improveR::setEditable(TRUE)
    TEST_FOLDER <- improveR:::emptyFolderSetup()
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


test_that("no logging", {
  TEST_FOLDER <- ensureTestFolder()
  FAKE_PATH <- getFakePath()
  expect_false("" == TEST_FOLDER)

  Sys.setenv(improver.logfile="")
  tryCatch({
    resource <- improveRcore::loadResource(FAKE_RES_ID)
  },
  error=function(e) {}
  ,finally = function() {
    logItems <- parseLogFile()
    expect_null(logItems)
    initImproveLogging("INFO")
    parseLogFile()
    expect_null(logItems)
  })
})


test_that("not connected|ics1081", {

  TEST_FOLDER <- ensureTestFolder()
  FAKE_PATH <- getFakePath()
  checkConnected <- function(func){
    expectedMessage <- "improveConnect was not called or an error was thrown while connecting"
    expectedError <- "not connected"
    tryCatch({
      resource <- func(FAKE_RES_ID)
    }, error=function(e) {
      expect_equal(as.character(e[1]),expectedError)
    },finally = function() {
      message <- improveLastLogMessage("ERROR")
      expect_equal(message,expectedMessage)
    })

    tryCatch({
      resource <- func(FAKE_ENTITY_ID)
    }, error=function(e) {
      expect_equal(as.character(e[1]),expectedError)
    },finally = function() {
      message <- improveLastLogMessage("ERROR")
      expect_equal(message,expectedMessage)
    })

    tryCatch({
      resource <- func(FAKE_LONG_ENTITY_ID)
    }, error=function(e) {
      expect_equal(as.character(e[1]),expectedError)
    },finally = function() {
      message <- improveLastLogMessage("ERROR")
      expect_equal(message,expectedMessage)
    })

    tryCatch({
      resource <- func(FAKE_PATH)
    }, error=function(e) {
      expect_equal(as.character(e[1]),expectedError)
    },finally = function() {
      message <- improveLastLogMessage("ERROR")
      expect_equal(message,expectedMessage)
    })


  }


  improveDisconnect()
  Sys.setenv(improver.logfile="improver.log")
  initImproveLogging("INFO")




  checkConnected(loadResource)
  checkConnected(loadAuditTrail)
  checkConnected(loadChildResources)
  checkConnected(loadChildSteps)
  checkConnected(loadFile)
  checkConnected(loadFullChildResources)
  checkConnected(loadHistory)
  checkConnected(loadMetaData)
  checkConnected(loadParentStep)
  checkConnected(loadReferences)


  checkConnected(unloadResource)
  checkConnected(unloadAuditTrail)
  checkConnected(unloadChildResources)
  checkConnected(unloadChildSteps)
  checkConnected(unloadFile)
  checkConnected(unloadFullChildResources)
  checkConnected(unloadHistory)
  checkConnected(unloadMetaData)
  checkConnected(unloadParentStep)
  checkConnected(unloadReferences)

  checkConnected(refreshResource)
  checkConnected(refreshAuditTrail)
  checkConnected(refreshChildResources)
  checkConnected(refreshChildSteps)
  checkConnected(refreshFile)
  checkConnected(refreshFullChildResources)
  checkConnected(refreshHistory)
  checkConnected(refreshMetaData)
  checkConnected(refreshParentStep)
  checkConnected(refreshReferences)


  ###queries
  expectedMessage <- "improveConnect was not called or an error was thrown while connecting"
  expectedError <- "not connected"

  tryCatch({
    query("Path='/'")
  }, error=function(e) {
    expect_equal(as.character(e[1]),expectedError)
  },finally = function() {
    message <- improveLastLogMessage("ERROR")
    expect_equal(message,expectedMessage)
  })

  tryCatch({
    queryFolder("Path='/'",ident = FAKE_ENTITY_ID)
  }, error=function(e) {
    expect_equal(as.character(e[1]),expectedError)
  },finally = function() {
    message <- improveLastLogMessage("ERROR")
    expect_equal(message,expectedMessage)
  })

  ###creates

  tryCatch({
    createAnalysisTree(FAKE_ENTITY_ID,"newTree")
  }, error=function(e) {
    expect_equal(as.character(e[1]),expectedError)
  },finally = function() {
    message <- improveLastLogMessage("ERROR")
    expect_equal(message,expectedMessage)
  })

  tryCatch({
    createExternalLink(FAKE_ENTITY_ID,"linkname","url")
  }, error=function(e) {
    expect_equal(as.character(e[1]),expectedError)
  },finally = function() {
    message <- improveLastLogMessage("ERROR")
    expect_equal(message,expectedMessage)
  })

  tryCatch({
    createFile(FAKE_ENTITY_ID,"filename","path")
  }, error=function(e) {
    expect_equal(as.character(e[1]),expectedError)
  },finally = function() {
    message <- improveLastLogMessage("ERROR")
    expect_equal(message,expectedMessage)
  })

  tryCatch({
    createFolder(FAKE_ENTITY_ID,"foldername")
  }, error=function(e) {
    expect_equal(as.character(e[1]),expectedError)
  },finally = function() {
    message <- improveLastLogMessage("ERROR")
    expect_equal(message,expectedMessage)
  })

  tryCatch({
    createLink(FAKE_ENTITY_ID,FAKE_RES_ID)
  }, error=function(e) {
    expect_equal(as.character(e[1]),expectedError)
  },finally = function() {
    message <- improveLastLogMessage("ERROR")
    expect_equal(message,expectedMessage)
  })

  tryCatch({
    createStep(FAKE_ENTITY_ID,"foldername")
  }, error=function(e) {
    expect_equal(as.character(e[1]),expectedError)
  },finally = function() {
    message <- improveLastLogMessage("ERROR")
    expect_equal(message,expectedMessage)
  })

})



test_that("load connected|ics1085,ics1090,ics1093,ics1094,ics1096,ics1097,ics1099,ics1088,ics1206", {
  TEST_FOLDER <- ensureTestFolder()
  FAKE_PATH <- getFakePath()

  checkNonexisting <- function(func){
    expectedMessage <- "Resource with ID: {id} could not be loaded"
    expectedPathMessage <- "Resource with path: {id} could not be loaded."

    id <- FAKE_RES_ID
    resource <- func(id)
    eMsg <- as.character(glue::glue(expectedMessage))
    message <- improveLastLogMessage("WARN")
    expect_equal(message,eMsg)

    id <- FAKE_ENTITY_ID
    resource <- func(id)
    eMsg <- as.character(glue::glue(expectedMessage))
    message <- improveLastLogMessage("WARN")
    expect_equal(message,eMsg)

    id <- FAKE_LONG_ENTITY_ID
    resource <- func(id)
    message <- improveLastLogMessage("WARN")
    expect_equal(message,eMsg)

    id <- FAKE_PATH
    resource <- func(id)
    eMsg <- as.character(glue::glue(expectedPathMessage))
    message <- improveLastLogMessage("WARN")
    expect_true(startsWith(message,eMsg))
  }


  Sys.setenv(improver.logfile="improver.log")
  improveConnect()
  createFolder(TEST_FOLDER)




  checkNonexisting(loadResource)
  checkNonexisting(loadAuditTrail)
  checkNonexisting(loadChildResources)
  checkNonexisting(loadChildSteps)
  checkNonexisting(loadFile)
  checkNonexisting(loadFullChildResources)
  checkNonexisting(loadHistory)
  checkNonexisting(loadMetaData)
  checkNonexisting(loadParentStep)
  checkNonexisting(loadReferences)

  checkNonexisting(unloadResource)
  checkNonexisting(unloadAuditTrail)
  checkNonexisting(unloadChildResources)
  checkNonexisting(unloadChildSteps)
  checkNonexisting(unloadFile)
  checkNonexisting(unloadFullChildResources)
  checkNonexisting(unloadHistory)
  checkNonexisting(unloadMetaData)
  checkNonexisting(unloadParentStep)
  checkNonexisting(unloadReferences)

  checkNonexisting(refreshResource)
  checkNonexisting(refreshAuditTrail)
  checkNonexisting(refreshChildResources)
  checkNonexisting(refreshChildSteps)
  checkNonexisting(refreshFile)
  checkNonexisting(refreshFullChildResources)
  checkNonexisting(refreshHistory)
  checkNonexisting(refreshMetaData)
  checkNonexisting(refreshParentStep)
  checkNonexisting(refreshReferences)

})


test_that("query errors|ics1143", {
  TEST_FOLDER <- ensureTestFolder()
  Sys.setenv(improver.logfile="improver.log")
  improveConnect()
  createFolder(TEST_FOLDER)
  query("pat='/'")
  message <- improveLastLogMessage("WARN")
  expect_true(startsWith(message,"Error in query:  unknown attribute pat"))
})

test_that("create in non existing targets|ics1101,ics1102,ics1103,ics1104,ics1138,ics1140", {
  TEST_FOLDER <- ensureTestFolder()
  Sys.setenv(improver.logfile="improver.log")
  improveConnect()
  createFolder(TEST_FOLDER)


  createAnalysisTree(FAKE_RES_ID,"newTree")
  message <- improveLastLogMessage("WARN")
  expect_true(startsWith(message,"Target does not exist"))

  createExternalLink(FAKE_RES_ID,"newExternalLink","url")
  message <- improveLastLogMessage("WARN")
  expect_true(startsWith(message,"Target does not exist"))

  createFile(FAKE_RES_ID,"newFile")
  message <- improveLastLogMessage("WARN")
  expect_true(startsWith(message,"Target does not exist"))

  createFolder(FAKE_RES_ID,"newFolder")
  message <- improveLastLogMessage("WARN")
  expect_true(startsWith(message,"Target does not exist"))

  createLink(FAKE_RES_ID,FAKE_RES_ID)
  message <- improveLastLogMessage("WARN")
  expect_true(startsWith(message,"Target does not exist"))

  createStep(FAKE_RES_ID)
  message <- improveLastLogMessage("WARN")
  expect_true(startsWith(message,"Target does not exist"))
})


test_that("create in wrong target|ics1101,ics1102,ics1103,ics1104,ics1138,ics1140", {

  TEST_FOLDER <- ensureTestFolder()
  createResourceInNonContainer <- function(testContainer) {
    expectedMessage <- "{target} is not an allowed target type for {type}"
    target <- testContainer$nodeType
    failed <- createAnalysisTree(testContainer,"errorTree")
    type<-"Analysis Tree"
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message,as.character(glue::glue(expectedMessage)))

    failed <- createExternalLink(testContainer,"errorLink","url")
    type<-"ExtLink"
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message,as.character(glue::glue(expectedMessage)))

    failed <- createStep(testContainer)
    type<-"Step"
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message,as.character(glue::glue(expectedMessage)))

    failed <- createLink(testContainer,TEST_FOLDER)
    type<-"Link"
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message,as.character(glue::glue(expectedMessage)))

    failed <- createFolder(testContainer,"errorFolder")
    type<-"Folder"
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message,as.character(glue::glue(expectedMessage)))

    failed <- createFile(testContainer,"errorFile")
    type<-"File"
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message,as.character(glue::glue(expectedMessage)))
  }



  Sys.setenv(improver.logfile="improver.log")
  improveConnect()
  createFolder(TEST_FOLDER)

  #create resources in tree

  expectedMessage <- "{target} is not an allowed target type for {type}"

  analysisTree <- createAnalysisTree(TEST_FOLDER,"errorTree")
  expect_equal(analysisTree$nodeType,"Analysis Tree")
  expect_equal(analysisTree$name,"errorTree")
  target <- analysisTree$nodeType

  failed <- createAnalysisTree(analysisTree,"errorTree")
  type<-"Analysis Tree"
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,as.character(glue::glue(expectedMessage)))

  failed <- createExternalLink(analysisTree,"errorLink","url")
  type<-"ExtLink"
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,as.character(glue::glue(expectedMessage)))

  #create resources in folder
  failed <- createStep(TEST_FOLDER,"errorStep")
  type<-"Step"
  target<-"Folder"
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,as.character(glue::glue(expectedMessage)))


  #create resource in step

  testStep <- createStep(analysisTree,toolId = NULL)
  #test wrong tool id
  expect_equal(testStep$nodeType,"Step")
  expect_equal(testStep$name,"Step 1")
  target <- testStep$nodeType

  failed <- createAnalysisTree(testStep,"errorTree")
  type<-"Analysis Tree"
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,as.character(glue::glue(expectedMessage)))

  failed <- createStep(testStep)
  type<-"Step"
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,as.character(glue::glue(expectedMessage)))

  #create resource in file

  testFile <- createFile(TEST_FOLDER,"testFile")
  expect_equal(testFile$nodeType,"File")
  expect_equal(testFile$name,"testFile")

  createResourceInNonContainer(testFile)

  #create resource in link

  testLink <- createLink(TEST_FOLDER,testFile,"testLink")
  expect_equal(testLink$nodeType,"Link")
  expect_equal(testLink$name,"testLink")

  createResourceInNonContainer(testLink)

  #create resource in extLink

  testLink <- createExternalLink(TEST_FOLDER,"testExtLink","http://scinteco.com")
  expect_equal(testLink$nodeType,"ExtLink")
  expect_equal(testLink$name,"testExtLink")

  createResourceInNonContainer(testLink)

})


test_that("other create errors|ics1101,ics1102,ics1103,ics1104,ics1138", {
  TEST_FOLDER <- ensureTestFolder()
  createWithWrongName <- function(testName) {
    expectedMessage <- "name needs to be of type character"

    failed <- createAnalysisTree(TEST_FOLDER,testName)
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message,expectedMessage)

    failed <- createFolder(TEST_FOLDER,testName)
    expect_null(failed)
    message <-improveLastLogMessage("WARN")
    expect_equal(message,expectedMessage)

    failed <- createFile(TEST_FOLDER,testName)
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message,expectedMessage)

    failed <- createLink(TEST_FOLDER,TEST_FOLDER,testName)
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message,expectedMessage)

    failed <- createExternalLink(TEST_FOLDER,testName,"http://scinteco.com")
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message,expectedMessage)
  }

  createWithWrongComment <- function(testComment) {
    testName <- "name"
    expectedMessage <- "comment needs to be of type character"

    failed <- createAnalysisTree(TEST_FOLDER,testName,comment=testComment)
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message,expectedMessage)

    failed <- createFolder(TEST_FOLDER,testName,comment=testComment)
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message,expectedMessage)

    failed <- createFile(TEST_FOLDER,testName,comment=testComment)
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message,expectedMessage)

    #Links do not do comments

    failed <- createExternalLink(TEST_FOLDER,testName,"http://scinteco.com",comment=testComment)
    expect_null(failed)
    message <- improveLastLogMessage("WARN")
    expect_equal(message,expectedMessage)
  }

  Sys.setenv(improver.logfile="improver.log")
  improveConnect()
  createFolder(TEST_FOLDER)

  #wrong name
  createWithWrongName(NULL)
  createWithWrongName(NA)
  createWithWrongName(23)
  createWithWrongName(data.frame())

  #wrong name
  createWithWrongComment(NULL)
  createWithWrongComment(NA)
  createWithWrongComment(23)
  createWithWrongComment(data.frame())

  #automatic name parsing

  expectedMessage<- "Target does not exist"
  expectedMessage2 <-"either full path with name of new resource or target path and resource name have to be provided"

  newPath <- paste0(TEST_FOLDER,"/{newName}")

  #tree

  newName <- "newTree"
  usePath <- glue::glue(newPath)
  testItem <- createAnalysisTree(usePath)
  expect_equal(testItem$nodeType,"Analysis Tree")
  expect_equal(testItem$path,usePath)

  newName <- "FAKE/newTree"
  usePath <- glue::glue(newPath)
  failed <- createAnalysisTree(usePath)
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,expectedMessage)

  usePath <- "FAKE"
  failed <- createAnalysisTree(usePath)
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,expectedMessage2)

  #folder

  newName <- "newFolder"
  usePath <- glue::glue(newPath)
  testItem <- createFolder(usePath)
  expect_equal(testItem$nodeType,"Folder")
  expect_equal(testItem$path,usePath)

  newName <- "FAKE/newFolder"
  usePath <- glue::glue(newPath)
  failed <- createFolder(usePath)
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,expectedMessage)

  usePath <- "FAKE"
  failed <- createFolder(usePath)
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,expectedMessage2)

  #file

  newName <- "newFile"
  usePath <- glue::glue(newPath)
  testItem <- createFile(usePath)
  expect_equal(testItem$nodeType,"File")
  expect_equal(testItem$path,usePath)

  newName <- "FAKE/newFile"
  usePath <- glue::glue(newPath)
  failed <- createFile(usePath)
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,expectedMessage)

  usePath <- "FAKE"
  failed <- createFolder(usePath)
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,expectedMessage2)

  newName <- "newFileWithLocal"
  usePath <- glue::glue(newPath)
  feiled <- createFile(usePath,localPath = "improver.log")
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,expectedMessage)

  #not possible for link as name of linked resource is used if no link name is given

  #ExtLink

  newName <- "newExt"
  usePath <- glue::glue(newPath)
  testItem <- createExternalLink(usePath,url = "http://scinteco.com")
  expect_equal(testItem$nodeType,"ExtLink")
  expect_equal(testItem$path,usePath)

  newName <- "FAKE/newExt"
  usePath <- glue::glue(newPath)
  failed <- createExternalLink(usePath,url = "http://scinteco.com")
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,expectedMessage)

  usePath <- "FAKE"
  failed <- createExternalLink(usePath)
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,expectedMessage2)

  #create multiple targets, multiple one does not exist, multiple resources ... TODO

  #already exists /  / create multiple res, check same name

  expectedAlreadyExists1 <- "{resName} already exists in {TEST_FOLDER}"
  expectedAlreadyExists2 <- "{resName} already exists in {TEST_FOLDER} but is of type {type}"

  #tree

  resName <- "tree1"
  testItem <- createAnalysisTree(TEST_FOLDER,resName)
  expect_equal(testItem$name,resName)
  expect_equal(testItem$nodeType,"Analysis Tree")
  testItem <- createAnalysisTree(TEST_FOLDER,resName)
  expect_equal(testItem$name,resName)
  expect_equal(testItem$nodeType,"Analysis Tree")
  message <- improveLastLogMessage("INFO")
  expect_equal(message,glue::glue(expectedAlreadyExists1))

  #folder

  resName <- "folder1"
  testItem <- createFolder(TEST_FOLDER,resName)
  expect_equal(testItem$name,resName)
  expect_equal(testItem$nodeType,"Folder")
  testItem <- createFolder(TEST_FOLDER,resName)
  expect_equal(testItem$name,resName)
  expect_equal(testItem$nodeType,"Folder")
  message <- improveLastLogMessage("INFO")
  expect_equal(message,glue::glue(expectedAlreadyExists1))

  #file

  resName <- "file1"
  testItem <- createFile(TEST_FOLDER,resName)
  expect_equal(testItem$name,resName)
  expect_equal(testItem$nodeType,"File")
  testItem <- createFile(TEST_FOLDER,resName)
  expect_equal(testItem$name,resName)
  expect_equal(testItem$nodeType,"File")
  message <- improveLastLogMessage("INFO")
  expect_equal(message,glue::glue(expectedAlreadyExists1))

  #link

  resName <- "link1"
  testItem <- createLink(TEST_FOLDER,TEST_FOLDER,resName)
  expect_equal(testItem$name,resName)
  expect_equal(testItem$nodeType,"Link")
  testItem <- createLink(TEST_FOLDER,TEST_FOLDER,resName)
  expect_equal(testItem$name,resName)
  expect_equal(testItem$nodeType,"Link")
  message <- improveLastLogMessage("INFO")
  expect_equal(message,glue::glue(expectedAlreadyExists1))

  #ExtLink

  resName <- "extlink1"
  testItem <- createExternalLink(TEST_FOLDER,resName,url="http://scinteco.com")
  expect_equal(testItem$name,resName)
  expect_equal(testItem$nodeType,"ExtLink")
  testItem <- createExternalLink(TEST_FOLDER,resName,url="http://scinteco.com")
  expect_equal(testItem$name,resName)
  expect_equal(testItem$nodeType,"ExtLink")
  message <- improveLastLogMessage("INFO")
  expect_equal(message,glue::glue(expectedAlreadyExists1))

  #already exists but other type

  #Analysis Tree

  resName <- "folder1"
  type <- "Folder"
  testItem <- createAnalysisTree(TEST_FOLDER,resName)
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))
  resName <- "file1"
  type <- "File"
  testItem <- createAnalysisTree(TEST_FOLDER,resName)
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))
  resName <- "link1"
  type <- "Link"
  testItem <- createAnalysisTree(TEST_FOLDER,resName)
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))
  resName <- "extlink1"
  type <- "ExtLink"
  testItem <- createAnalysisTree(TEST_FOLDER,resName)
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))

  #Folder

  resName <- "tree1"
  type <- "Analysis Tree"
  testItem <- createFolder(TEST_FOLDER,resName)
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))
  resName <- "file1"
  type <- "File"
  testItem <- createFolder(TEST_FOLDER,resName)
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))
  resName <- "link1"
  type <- "Link"
  testItem <- createFolder(TEST_FOLDER,resName)
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))
  resName <- "extlink1"
  type <- "ExtLink"
  testItem <- createFolder(TEST_FOLDER,resName)
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))

  #File

  resName <- "tree1"
  type <- "Analysis Tree"
  testItem <- createFile(TEST_FOLDER,resName)
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))
  resName <- "folder1"
  type <- "Folder"
  testItem <- createFile(TEST_FOLDER,resName)
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))
  resName <- "link1"
  type <- "Link"
  testItem <- createFile(TEST_FOLDER,resName)
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))
  resName <- "extlink1"
  type <- "ExtLink"
  testItem <- createFile(TEST_FOLDER,resName)
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))

  #Link

  resName <- "tree1"
  type <- "Analysis Tree"
  testItem <- createLink(TEST_FOLDER,TEST_FOLDER,resName)
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))
  resName <- "folder1"
  type <- "Folder"
  testItem <- createLink(TEST_FOLDER,TEST_FOLDER,resName)
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))
  resName <- "file1"
  type <- "File"
  testItem <- createLink(TEST_FOLDER,TEST_FOLDER,resName)
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))
  resName <- "extlink1"
  type <- "ExtLink"
  testItem <- createLink(TEST_FOLDER,TEST_FOLDER,resName)
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))

  #ExternalLink

  resName <- "tree1"
  type <- "Analysis Tree"
  testItem <- createExternalLink(TEST_FOLDER,resName,url="http://scinteco.com")
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))
  resName <- "folder1"
  type <- "Folder"
  testItem <- createExternalLink(TEST_FOLDER,resName,url="http://scinteco.com")
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))
  resName <- "file1"
  type <- "File"
  testItem <- createExternalLink(TEST_FOLDER,resName,url="http://scinteco.com")
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))
  resName <- "link1"
  type <- "Link"
  testItem <- createExternalLink(TEST_FOLDER,resName,url="http://scinteco.com")
  expect_null(testItem)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,glue::glue(expectedAlreadyExists2))

  ##########################################illegal local path

  expectedNonExistant <- "localPath ./nonExistant does not exist"

  failed <- createFile(TEST_FOLDER,fileName="withName",localPath="./nonExistant")
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,expectedNonExistant)

  failed <- createFile(TEST_FOLDER,localPath="./nonExistant")
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,expectedNonExistant)

  #illegal link

  expectedInvalidLinkTarget <- "LinkTarget does not exist"

  failed <- createLink(TEST_FOLDER,FAKE_RES_ID,linkName="fake")
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,expectedInvalidLinkTarget)

  failed <- createLink(TEST_FOLDER,FAKE_RES_ID)
  expect_null(failed)
  message <- improveLastLogMessage("WARN")
  expect_equal(message,expectedInvalidLinkTarget)

})


#get stuff
#metadata
#move all loads for processes, ... to improveRcore
#review
#wrong pwd, first rename to pwd, but then count up the version (instead of fromForRelativeLink)
# setup of all test folders
#recursive create

#create file with same signature as link
#test for parent step and create
#improveLogin with username, get key
