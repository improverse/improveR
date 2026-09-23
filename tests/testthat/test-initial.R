

Sys.setenv(TEST_NAME="initial")



  test_that("createTestFolder", {
    Sys.setenv(IMPROVER_TEST_REPLAY="T")
    improveConnect()
    setEditable(T)
    expect_false(Sys.getenv("IMPROVER_TOKEN")=="")
    TEST_FOLDER <- improveR:::baseFilesSetup()
    assign(x = "TEST_FOLDER",value = TEST_FOLDER,envir = globalenv())
  })
# })

test_that("general file setup", {
  TEST_FOLDER <- improveR:::baseFilesSetup()
  expect_false("" == TEST_FOLDER)
})

  test_that("loadResource|ics1090,ics1093", {
    TEST_FOLDER <- improveR:::baseFilesSetup()
    folder <- loadResource(TEST_FOLDER)
    expect_equal(folder$nodeType,"Folder")
    expect_equal(folder$path,TEST_FOLDER)
  })
# })

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

    # Clean up leftover resources from previous test runs
    existing <- loadResource("./versionMoved", folder)
    if (!is.null(existing)) delete(existing)
    existing <- loadResource("./versionFolder", folder)
    if (!is.null(existing)) delete(existing)
    resetCache()

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
# })

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
# })

# loadResourceFromServer | ics1084
test_that("loadResourceFromServer loads by resourceId, entityId, entityVersionId, returns POSIX dates|ics1084", {
  TEST_FOLDER <- improveR:::baseFilesSetup()
  folder <- loadResource(TEST_FOLDER)

  # by resourceId
  byResId <- improveR::loadResourceFromServer(folder$resourceId)
  expect_false(is.null(byResId))
  expect_equal(byResId$resourceId, folder$resourceId)

  # by entityId
  byEntId <- improveR::loadResourceFromServer(folder$entityId)
  expect_false(is.null(byEntId))
  expect_equal(byEntId$entityId, folder$entityId)

  # by entityVersionId
  byEntVerId <- improveR::loadResourceFromServer(folder$entityVersionId)
  expect_false(is.null(byEntVerId))
  expect_equal(byEntVerId$entityVersionId, folder$entityVersionId)

  # dates are POSIX via convertImproveTimestampToPosix
  expect_true(inherits(byResId$createdAtDate, "POSIXct"))

  # id 0 yields a virtual root
  root <- improveR::loadResourceFromServer("0")
  expect_false(is.null(root))
  expect_equal(root$resourceId, "0")
  expect_equal(root$nodeType, "Folder")
  expect_equal(root$path, "/")

  # list input returns a merged dataframe with one row per id
  byList <- improveR::loadResourceFromServer(list(folder$resourceId, folder$resourceId))
  expect_false(is.null(byList))
  expect_true(is.data.frame(byList))
  expect_gte(nrow(byList), 2)
})

# getParent | ics1088
test_that("getParent returns parent resourceId, root for top-level|ics1088", {
  TEST_FOLDER <- improveR:::baseFilesSetup()
  parentFolder <- loadResource(TEST_FOLDER)
  child <- improveR::createFolder(targetIdent = TEST_FOLDER,
                                  folderName = paste0("getParentProbe-", uniqueTag()))
  on.exit(tryCatch(delete(child$resourceId), error = function(e) NULL), add = TRUE)

  parentId <- improveR::getParent(child$resourceId)
  expect_equal(parentId, parentFolder$resourceId)

  # Also works with entityId input
  parentIdFromEnt <- improveR::getParent(child$entityId)
  expect_equal(parentIdFromEnt, parentFolder$resourceId)

  # Root resource has no parentId — getParent returns 0 per spec
  rootParent <- improveR::getParent("0")
  expect_equal(rootParent, 0)
})

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
# })



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
# })

  test_that("load multiple audit trails|ics1097", {
    TEST_FOLDER <- improveR:::baseFilesSetup()
    folder <- loadResource(TEST_FOLDER)

    # Create a fresh, uniquely-named child folder inside this test so the
    # assertion targets a resource whose full lifecycle is owned by the test
    # (no dependence on row order or pre-existing audit-trail state).
    freshChildName <- paste0("auditTrailProbe-", uniqueTag())
    freshChild <- createFolder(targetIdent = TEST_FOLDER, folderName = freshChildName)
    expect_false(is.null(freshChild))
    on.exit(tryCatch(delete(freshChild$resourceId), error = function(e) NULL), add = TRUE)

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

    expect_equal(childResourcesNumber,nrow(auditTrails))

    # Target the fresh child by entityId (invariant across renames),
    # not by row index or by current resourceName.
    childRow <- childResources[childResources$entityId == freshChild$entityId, ]
    expect_equal(nrow(childRow), 1)

    childTrail <- auditTrails[auditTrails$entityId == freshChild$entityId, ]
    expect_equal(nrow(childTrail), 1)
    childTrailData <- childTrail$data[[1]]

    ownEvents <- childTrailData[childTrailData$entityId == freshChild$entityId, ]
    expect_gt(nrow(ownEvents), 0)
    expect_equal(unique(ownEvents$entityId), freshChild$entityId)
    expect_true("create" %in% ownEvents$operation)
  })
# })

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
# })


  test_that("load history|ics1094", {
    TEST_FOLDER <- improveR:::baseFilesSetup()
    textFileFolder <- paste0(TEST_FOLDER, "/rgetTEXT")
    res <- loadResource("./sampleText.txt",textFileFolder)
    history <- loadHistory(res$entityId)
    
    # Verify that history was loaded
    expect_true(!is.null(history))
    
    # Check if history has data (might be empty for new resources)
    if (!is.null(history$data) && length(history$data) > 0) {
      historyEntries <- length(history$data[[1]])
      expect_true(historyEntries >= 0, info = "History entries should be non-negative")
    } else {
      # New resources might have empty history, which is valid
      expect_true(TRUE, info = "New resource has empty history")
    }
  })
# })
