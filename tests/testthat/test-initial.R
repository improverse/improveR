

Sys.setenv(TEST_NAME="initial")



httptest::with_mock_dir("prepare-initial",{
  test_that("createTestFolder", {
    Sys.setenv(IMPROVER_TEST_REPLAY="T")
    improveConnect()
    setEditable(T)
    expect_false(Sys.getenv("IMPROVER_TOKEN")=="")
    TEST_FOLDER <- improveR:::baseFilesSetup()
    assign(x = "TEST_FOLDER",value = TEST_FOLDER,envir = globalenv())
  })
})

test_that("general file setup", {
  TEST_FOLDER <- improveR:::baseFilesSetup()
  expect_false("" == TEST_FOLDER)
})

httptest::with_mock_dir("loadResource",{
  test_that("loadResource|ics1090,ics1093", {
    TEST_FOLDER <- improveR:::baseFilesSetup()
    folder <- loadResource(TEST_FOLDER)
    expect_equal(folder$nodeType,"Folder")
    expect_equal(folder$path,TEST_FOLDER)
  })
})

httptest::with_mock_dir("testCaching",{
  test_that("test Caching|ics1091,ics1090,ics1093", {
    TEST_FOLDER <- improveR:::baseFilesSetup()
    improveConnect(persistentCaching = T)
    folder <- loadResource(TEST_FOLDER)

  cacheEnv<-improveR:::cacheEnv

    resourceIdCache <-cacheEnv$resourceIdCache
    resourceEntityIdCache <- cacheEnv$resourceEntityIdCache
    resourceEntityVersionIdCache <- cacheEnv$resourceEntityVersionIdCache
    resourcePathCache<-cacheEnv$resourcePathCache

    expect_equal(folder, resourceIdCache[[folder$resourceId]])
    expect_equal(folder,resourceEntityIdCache[[folder$entityId]])
    expect_equal(folder,resourcePathCache[[folder$path]])

    improveR::resetCache()

    resourceIdCache <-cacheEnv$resourceIdCache
    resourceEntityIdCache <- cacheEnv$resourceEntityIdCache
    resourceEntityVersionIdCache <- cacheEnv$resourceEntityVersionIdCache
    resourcePathCache<-cacheEnv$resourcePathCache

    expect_equal(NULL, resourceIdCache[[folder$resourceId]])
    expect_equal(NULL,resourceEntityIdCache[[folder$entityId]])
    expect_equal(NULL,resourcePathCache[[folder$path]])

    folder <- improveR::loadResource(folder)

    resourceIdCache <-cacheEnv$resourceIdCache
    resourceEntityIdCache <- cacheEnv$resourceEntityIdCache
    resourceEntityVersionIdCache <- cacheEnv$resourceEntityVersionIdCache
    resourcePathCache<-cacheEnv$resourcePathCache

    expect_equal(folder, resourceIdCache[[folder$resourceId]])
    expect_equal(folder, resourceEntityVersionIdCache[[folder$entityVersionId]])
    expect_equal(folder,resourceEntityIdCache[[folder$entityId]])
    expect_equal(folder,resourcePathCache[[folder$path]])

    resetCache()

    resetCache()
    cacheEnv<-improveR:::cacheEnv
    folder <- loadResource(TEST_FOLDER)
    newFolder <- createFolder(folder,"versionFolder")

    firstEntityVersionId <- newFolder$entityVersionId
    movedFolder <- move(newFolder,folder,targetName = "versionMoved")
    secondEntityVersionId <- movedFolder$entityVersionId
    entityId <- newFolder$entityId


    oldVersion <- loadResource(firstEntityVersionId)

    resourceIdCache <-cacheEnv$resourceIdCache
    resourceEntityIdCache <- cacheEnv$resourceEntityIdCache
    resourceEntityVersionIdCache <- cacheEnv$resourceEntityVersionIdCache
    resourcePathCache<-cacheEnv$resourcePathCache
    versionedresourceEntityVersionIdCache<-cacheEnv$versionedresourceEntityVersionIdCache


    expect_equal(newFolder$entityVersionId,versionedresourceEntityVersionIdCache[[firstEntityVersionId]]$entityVersionId)
    expect_equal(newFolder$name,versionedresourceEntityVersionIdCache[[firstEntityVersionId]]$name)
    expect_true(versionedresourceEntityVersionIdCache[[firstEntityVersionId]]$isVersion)

    newVersion <- loadResource(secondEntityVersionId)

    expect_equal(movedFolder$entityVersionId,versionedresourceEntityVersionIdCache[[secondEntityVersionId]]$entityVersionId)
    expect_equal(movedFolder$name,versionedresourceEntityVersionIdCache[[secondEntityVersionId]]$name)
    expect_true(versionedresourceEntityVersionIdCache[[secondEntityVersionId]]$isVersion)
  })
})

httptest::with_mock_dir("getCorrectID",{
  test_that("get correct ID|ics1087" , {
    TEST_FOLDER <- improveR:::baseFilesSetup()
    folder <- loadResource(TEST_FOLDER)

    expect_equal(folder$resourceId,getCorrectId(folder))

    weirdPath1 <- "/path/mat&methods/name:this/§34/a=b"
    expect_equal(weirdPath1,getCorrectId(weirdPath1))
    weirdPath2 <- paste0(".",weirdPath1)
    expect_equal(weirdPath2,getCorrectId(weirdPath2))

    entityId <- folder$entityId
    prefixLessEntityId <- strsplit(entityId,":",fixed=T)[[1]][2]
    longEntityId <- paste0(
      "https://whatever/resolveto=",
      entityId
    )

    expect_equal(entityId,getCorrectId(entityId))
    expect_equal(entityId,getCorrectId(prefixLessEntityId))
    expect_equal(entityId,getCorrectId(longEntityId))
  })
})

httptest::with_mock_dir("normalisePath",{
  test_that("normalise path|ics1089", {
    TEST_FOLDER <- improveR:::baseFilesSetup()
    #implement function for adding new relations like ... for parent step
    path<-"./../lmer/../lmer"
    expect_equal("/lmer",normalisePath(path))



    path<-"./../lmer/../lmer"
    startPath <- "/0demo/lmer"
    #expected: /0demo/lmer
    expect_equal("/0demo/lmer",normalisePath(path,startPath))

    path<-"./../../../lmer/../lmer"
    startPath <- "/0demo/lmer"
    expect_equal(NULL,normalisePath(path,startPath))


    path<-"/0demo/lmer"
    startPath <- "/0demo/lmer"
    #expected: /0demo/lmer
    expect_equal("/0demo/lmer",normalisePath(path,startPath))

    path<-"/0demo/../0demo/lmer/../lmer"
    startPath <- "/0demo/lmer"
    #expected: /0demo/lmer
    expect_equal("/0demo/lmer",normalisePath(path,startPath))
  })
})



httptest::with_mock_dir("loadAuditTrail",{
  test_that("load audit trail|ics1097", {
    TEST_FOLDER <- improveR:::baseFilesSetup()
    folder <- loadResource(TEST_FOLDER)
    auditTrail <- loadAuditTrail(folder)
    expect_equal(auditTrail$type,"auditTrail")
    expect_equal(auditTrail$resourceId,folder$resourceId)
    expect_equal(auditTrail$entityId,folder$entityId)
    expect_equal(auditTrail$entityVersionId,folder$entityVersionId)
    expect_equal(auditTrail$path,folder$path)

    auditTrailData <- auditTrail$data[[1]]

    withoutChildren <- auditTrailData[auditTrailData$resourceName==folder$name,]
    expect_gte(nrow(withoutChildren),0)
    expect_equal(unique(withoutChildren$entityId),folder$entityId)
    expect_true("create" %in% withoutChildren$operation)
  })
})

httptest::with_mock_dir("loadMultipleAuditTrails",{
  test_that("load multiple audit trails|ics1097", {
    TEST_FOLDER <- improveR:::baseFilesSetup()
    folder <- loadResource(TEST_FOLDER)
    children <- loadChildResources(folder)
    expect_equal(children$type,"child")
    expect_equal(children$resourceId,folder$resourceId)
    expect_equal(children$entityId,folder$entityId)
    expect_equal(children$entityVersionId,folder$entityVersionId)
    expect_equal(children$path,folder$path)
    expect_equal(children$name,folder$name)

    childResources <- children$data[[1]]

    childResourcesNumber <- nrow(childResources)
    auditTrails <- loadAuditTrail(childResources)

    #auditTrails <- improveR:::mergeDataframeList(auditTrails)

    expect_equal(childResourcesNumber,nrow(auditTrails))

    childResource <- childResources[2,]

    childTrail <- auditTrails[auditTrails$entityId==childResource$entityId,]
    childTrailData <- childTrail$data[[1]]

    withoutChildren <- childTrailData[childTrailData$resourceName==childResource$name,]
    expect_gte(nrow(withoutChildren),0)
    expect_equal(unique(withoutChildren$entityId),childResource$entityId)
    expect_true("create" %in% withoutChildren$operation)
  })
})

httptest::with_mock_dir("loadFile",{
  test_that("load file|ics1099", {
    TEST_FOLDER <- improveR:::baseFilesSetup()
    textFileFolder <- paste0(TEST_FOLDER, "/rgetTEXT")
    res <- loadResource("./sampleText.txt",textFileFolder)

    res <- loadResource("./sampleText.txt",textFileFolder)
    # add error message that ./ is required before paths


    expect_equal(loadFile(res$resourceId),loadFile(res$entityId))

    # Cross-validation with entityVersionId
    expect_equal(loadFile(res$resourceId),loadFile(res$entityVersionId))

    # if invalid input (e.g. try res$resourceID) error message is cryptic -> user would have to investigate in source code

    fileWithoutParams <- loadFile(res$resourceId)
    firstPath <- fileWithoutParams$data[[1]]
    resetCache()
    fileWithID <- loadFile(res$resourceId, addIdToName = T)
    secondPath <- fileWithID$data[[1]]

    expect_false(firstPath==secondPath)

    tools::md5sum(firstPath) == tools::md5sum(secondPath)

    unlink(firstPath)
    unlink(secondPath)

  })
})


httptest::with_mock_dir("loadHistory",{
  test_that("load history|ics1094", {
    TEST_FOLDER <- improveR:::baseFilesSetup()
    textFileFolder <- paste0(TEST_FOLDER, "/rgetTEXT")
    res <- loadResource("./sampleText.txt",textFileFolder)
    history <- loadHistory(res$entityId)
    historyEntries <- length(history$data[[1]])

  })
})
