test_that("loadResource|ics1090,ics1093", {
  TEST_FOLDER <- baseFilesSetup()
  folder <- improveR::loadResource(TEST_FOLDER)
  expect_equal(folder$nodeType,"Folder")
  expect_equal(folder$path,TEST_FOLDER)

})