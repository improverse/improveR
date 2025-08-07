Sys.setenv(TEST_NAME="metadata")



httptest::with_mock_dir("prepare-metadata",{
  test_that("createTestFolder", {
    Sys.setenv(IMPROVER_TEST_REPLAY="T")
    improveConnect()
    setEditable(T)
    expect_false(Sys.getenv("IMPROVER_TOKEN")=="")
    TEST_FOLDER <- baseFilesSetup()
    assign(x = "TEST_FOLDER",value = TEST_FOLDER,envir = globalenv())
  })
})

httptest::with_mock_dir("createFolderForMetadata", {
  test_that("create Folder for metadata", {
    metadatafolder <- createFolder(TEST_FOLDER, "metadata")

    testFiles <- loadChildResources("./rgetGRAPH", from = TEST_FOLDER) %>% strip()
    copied <- copy(sources = testFiles, metadatafolder)

    expect_equal(nrow(copied), 3)
  })
})

httptest::with_mock_dir("checkIfMetadataDefinitionsExist", {
  test_that("check if metadata definitions exist|ics1096", {
    definitions <- loadMetaDataDefinitions()
    compound <- definitions[definitions$name == "Compound", ]
    expect_equal(nrow(compound), 1)
    expect_equal(compound$metadataType, "TEXT")

    indication <- definitions[definitions$name == "Indication", ]
    expect_equal(nrow(indication), 1)
    expect_equal(indication$metadataType, "LOV")

    programStart <- definitions[definitions$name == "ProgramStart", ]
    expect_equal(nrow(programStart), 1)
    expect_equal(programStart$metadataType, "DATE")
  })
})

httptest::with_mock_dir("createLoadUpdateAndDeleteMetadataForOneFolder", {
  test_that("create, load, update and delete metadata for one folder|ics1096,ics1137", {
    metadatafolder <- loadResource("./metadata", TEST_FOLDER)

    result <- addMetaDate(metadatafolder, "Compound", value = "Compound")
    result <- addMetaDate(metadatafolder, "Indication", value = "Cancer")
    result <- addMetaDate(metadatafolder, "ProgramStart", value = lubridate::ymd("2017-01-30"))

    metaData <- loadMetaData(metadatafolder)
    expect_equal(metaData$type, "meta data")
    expect_equal(metaData$resourceId, metadatafolder$resourceId)
    expect_equal(metaData$entityId, metadatafolder$entityId)
    expect_equal(metaData$entityVersionId, metadatafolder$entityVersionId)
    expect_equal(metaData$path, metadatafolder$path)
    expect_equal(metaData$name, metadatafolder$name)

    metaDataData <- metaData %>% strip()

    expect_equal(
      metaDataData[metaDataData$descriptorName == "Compound", ]$textValue,
      "Compound"
    )

    expect_equal(
      metaDataData[metaDataData$descriptorName == "Indication", ]$lovText,
      "Cancer"
    )

    expect_equal(
      lubridate::ymd(metaDataData[metaDataData$descriptorName == "ProgramStart", ]$dateValueDate),
      lubridate::ymd("2017-01-30")
    )

    a <- updateMetaDate(metadatafolder, "Compound", "Compound1")
    a <- updateMetaDate(metadatafolder, "Indication", "Diabetes")
    a <- updateMetaDate(metadatafolder, "ProgramStart", lubridate::ymd("2017-01-31"))

    metaData <- updateMetaData(metadatafolder)
    expect_equal(metaData$type, "meta data")
    expect_equal(metaData$resourceId, metadatafolder$resourceId)
    expect_equal(metaData$entityId, metadatafolder$entityId)
    expect_equal(metaData$entityVersionId, metadatafolder$entityVersionId)
    expect_equal(metaData$path, metadatafolder$path)
    expect_equal(metaData$name, metadatafolder$name)

    metaDataData <- metaData %>% strip()

    expect_equal(
      metaDataData[metaDataData$descriptorName == "Compound", ]$textValue,
      "Compound1"
    )

    expect_equal(
      metaDataData[metaDataData$descriptorName == "Indication", ]$lovText,
      "Diabetes"
    )

    expect_equal(
      lubridate::ymd(metaDataData[metaDataData$descriptorName == "ProgramStart", ]$dateValueDate),
      lubridate::ymd("2017-01-31")
    )

    a <- deleteMetaDate(metadatafolder, "Compound")
    a <- deleteMetaDate(metadatafolder, "Indication")
    a <- deleteMetaDate(metadatafolder, "ProgramStart")
  })
})

httptest::with_mock_dir("addAndDeleteBulkMetadataForOneFolder", {
  test_that("add and delete bulk metadata for one folder|ics1096,ics1137", {
    metadatafolder <- loadResource("./metadata", TEST_FOLDER)

    descriptorNameValueList <- list(
      list(descriptorName = "Compound", value = "Compound"),
      list(descriptorName = "Indication", value = "Diabetes"),
      list(descriptorName = "ProgramStart", value = lubridate::ymd("2017-01-31"))
    )
    result <- addBulkMetaDate(metadatafolder, descriptorNameValueList)

    metaData <- updateMetaData(metadatafolder)
    expect_equal(metaData$type, "meta data")
    expect_equal(metaData$resourceId, metadatafolder$resourceId)
    expect_equal(metaData$entityId, metadatafolder$entityId)
    expect_equal(metaData$entityVersionId, metadatafolder$entityVersionId)
    expect_equal(metaData$path, metadatafolder$path)
    expect_equal(metaData$name, metadatafolder$name)

    metaDataData <- metaData %>% strip()

    expect_equal(
      metaDataData[metaDataData$descriptorName == "Compound", ]$textValue,
      "Compound"
    )

    expect_equal(
      metaDataData[metaDataData$descriptorName == "Indication", ]$lovText,
      "Diabetes"
    )

    expect_equal(
      lubridate::ymd(metaDataData[metaDataData$descriptorName == "ProgramStart", ]$dateValueDate),
      lubridate::ymd("2017-01-31")
    )

    result <- deleteMetaDate(metadatafolder, c("Compound", "Indication", "ProgramStart"))

    metaData <- updateMetaData(metadatafolder)
    expect_equal(metaData$type, "meta data")
    expect_equal(metaData$resourceId, metadatafolder$resourceId)
    expect_equal(metaData$entityId, metadatafolder$entityId)
    expect_equal(metaData$entityVersionId, metadatafolder$entityVersionId)
    expect_equal(metaData$path, metadatafolder$path)
    expect_equal(metaData$name, metadatafolder$name)

    metaDataData <- metaData %>% strip()

    expect_equal(nrow(metaDataData), 0)
  })
})

httptest::with_mock_dir("metadataOnMultipleResourcesAtOnce", {
  test_that("metadata on multiple resources at once|ics1096,ics1137", {
    metadatafolder <- loadResource("./metadata", TEST_FOLDER)

    multiFiles <- loadChildResources(metadatafolder) %>% strip()

    result <- addMetaDate(multiFiles, "Compound", value = "Compound")
    result <- addMetaDate(multiFiles, "Indication", value = "Cancer")
    result <- addMetaDate(multiFiles, "ProgramStart", value = Sys.Date())

    metaData <- loadMetaData(multiFiles)
    expect_equal(nrow(metaData), nrow(multiFiles))

    sampleFile <- multiFiles[2, ]
    sampleMeta <- metaData[metaData$resourceId == sampleFile$resourceId, ]

    expect_equal(sampleMeta$type, "meta data")
    expect_equal(sampleMeta$resourceId, sampleFile$resourceId)
    expect_equal(sampleMeta$entityId, sampleFile$entityId)
    expect_equal(sampleMeta$entityVersionId, sampleFile$entityVersionId)
    expect_equal(sampleMeta$path, sampleFile$path)
    expect_equal(sampleMeta$name, sampleFile$name)

    metaDataData <- sampleMeta %>% strip()

    expect_equal(
      metaDataData[metaDataData$descriptorName == "Compound", ]$textValue,
      "Compound"
    )

    expect_equal(
      metaDataData[metaDataData$descriptorName == "Indication", ]$lovText,
      "Cancer"
    )

    expect_equal(
      lubridate::ymd(metaDataData[metaDataData$descriptorName == "ProgramStart", ]$dateValueDate),
      lubridate::ymd(Sys.Date())
    )

    a <- deleteMetaDate(multiFiles, "Compound")
    a <- deleteMetaDate(multiFiles, "Indication")
    a <- deleteMetaDate(multiFiles, "ProgramStart")

    resetCache()
    metaData <- loadMetaData(multiFiles)
    expect_equal(nrow(metaData), nrow(multiFiles))

    sampleMeta <- metaData[metaData$resourceId == sampleFile$resourceId, ]
    sampleData <- sampleMeta %>% strip()
    expect_equal(0, nrow(sampleData))
  })
})

httptest::with_mock_dir("addAndDeleteBulkMetadataForMultipleFiles", {
  test_that("add and delete bulk metadata for multiple files|ics1096,ics1137", {
    metadatafolder <- loadResource("./metadata", TEST_FOLDER)

    multiFiles <- loadChildResources(metadatafolder) %>% strip()

    descriptorNameValueList <- list(
      list(descriptorName = "Compound", value = "Compound"),
      list(descriptorName = "Indication", value = "Diabetes"),
      list(descriptorName = "ProgramStart", value = lubridate::ymd("2017-01-31"))
    )
    result <- addBulkMetaDate(multiFiles, descriptorNameValueList)
    resetCache()
    metaData <- loadMetaData(multiFiles)
    expect_equal(nrow(metaData), nrow(multiFiles))

    sampleFile <- multiFiles[2, ]
    sampleMeta <- metaData[metaData$resourceId == sampleFile$resourceId, ]

    expect_equal(sampleMeta$type, "meta data")
    expect_equal(sampleMeta$resourceId, sampleFile$resourceId)
    expect_equal(sampleMeta$entityId, sampleFile$entityId)
    expect_equal(sampleMeta$entityVersionId, sampleFile$entityVersionId)
    expect_equal(sampleMeta$path, sampleFile$path)
    expect_equal(sampleMeta$name, sampleFile$name)

    metaDataData <- metaData %>% strip()

    expect_equal(
      metaDataData[metaDataData$descriptorName == "Compound", ]$textValue,
      "Compound"
    )

    expect_equal(
      metaDataData[metaDataData$descriptorName == "Indication", ]$lovText,
      "Diabetes"
    )

    expect_equal(
      lubridate::ymd(metaDataData[metaDataData$descriptorName == "ProgramStart", ]$dateValueDate),
      lubridate::ymd("2017-01-31")
    )

    result <- deleteMetaDate(multiFiles, c("Compound", "Indication", "ProgramStart"))

    resetCache()
    metaData <- loadMetaData(multiFiles)
    expect_equal(nrow(metaData), nrow(multiFiles))

    sampleFile <- multiFiles[2, ]
    sampleMeta <- metaData[metaData$resourceId == sampleFile$resourceId, ]

    metaDataData <- sampleMeta %>% strip()

    expect_equal(nrow(metaDataData), 0)
  })
})
