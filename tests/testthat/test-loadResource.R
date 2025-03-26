test_that("loadResource correctly loads resources", {
  
  
  TEST_FOLDER <- baseFilesSetup()
  
  folder <- improveR::loadResource(TEST_FOLDER)
  expect_equal(folder$nodeType,"Folder")
  expect_equal(folder$path,TEST_FOLDER)

})

#loadResource #TODO
## resources can be loaded by three different functions => needs three mockings
## loadResourceByPathGeneric
## internalLoadResourceVersionFromServer
## internalLoadResourceFromServer
## these functions are called by getFromCache
## three different test functions?

## Path
# test_that("loadResources from a path", {

# #Create mocked cache environement from which resources are loaded; mocks response from getFromCache
# local_mocked_bindings(
#   getFromCache=function() 
#   res=data.frame(resourceId=c("res1", "res2", "res3"), row_id=seq(1:3), value=letters[1:3])
#   )

#   result <- loadResource("test/path")
#   expect_equal(nrow(result), 3)
#   expect_equal(result$resourceId, c("res1", "res2", "res3"))
#   expect_equal(result$value, letters[1:3])

# })
