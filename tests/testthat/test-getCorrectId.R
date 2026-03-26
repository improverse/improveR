#Dataframes
test_that("getCorrectId returns correct ID for provideded data frames", {

  dfId <- data.frame(resourceId = "100", details = "txt")
  expect_equal(getCorrectId(dfId), "100")

  dfIdMulti <- data.frame(resourceId = c("100", "200"), details = "txt")
  expect_equal(getCorrectId(dfIdMulti), c("100","200"))
  
  dfWoId <- data.frame(details = "txt")
  expect_error(getCorrectId(dfWoId), "missing resourceId")
})


#Paths, resourceVersion, fullEntityId,
##NOTE Not clear which input reaches last else if condition; 

test_that("getCorrectId returns correct id for provided location, resource (version) id, location ids, and full location", {

# else if (startsWith(resolveToId, "/") | startsWith(resolveToId, "./") | startsWith(resolveToId, "\\") | startsWith(resolveToId, ".\\")) 
# = location
location <- "/Projects/Roland/test/Step 12"
expect_equal(getCorrectId(location), "/Projects/Roland/test/Step 12")

# else if (!grepl("=", resolveToId, fixed = T) &&
#         !grepl(":", resolveToId, fixed = T) &&
#         !grepl("-", resolveToId, fixed = T)) 
# = Resource (Version id)
resourceVersionId <- "5EF0E9458BAB459293EA42CC32BF2B6C"
expect_equal(getCorrectId(resourceVersionId), "5EF0E9458BAB459293EA42CC32BF2B6C")

## else if (grepl("=", resolveToId, fixed = T)), e.g. full entity (version) Id
fullEntityId <- "http://localhost:3000/provide?resourceId=resourceId=robert_oracle-1:ST-63678"
expect_equal(getCorrectId(fullEntityId), "robert_oracle-1:ST-63678")

## else if (!grepl(":", resolveToId, fixed = T)) #TODO not clear which input reaches last else if clause
## has to full fille also preceding else if conditions, otherwise never reaches the condition intended to test
# pathToTest <- "http://localhost:3000/provide?resourceId=resourceId=robert_oracle-1:ST-63678"
# expect_equal(getCorrectId(pathToTest), "robert_oracle-1:ST-63678")

})


