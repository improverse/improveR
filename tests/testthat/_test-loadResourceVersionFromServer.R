# #Add
# # test that idents longer than 2 work and return a dataframe
#
# #EXTRACTENTITYID (only short)
# test_that("Does extractEntityId extract an entityId correctly from an entityVersionId?", {
#
# shortEntityVersionId <- "robert_oracle-1:ST-63657-1"
# # fullEntityVersionId <- "http://localhost:3000/provide?resourceId=resourceId=robert_oracle-1:ST-63657-1"
#
# expect_true(extractEntityId(shortEntityVersionId) == "robert_oracle-1:ST-63657")
# # expect_true(extractEntityId(fullEntityVersionId) == "http://localhost:3000/provide?resourceId=resourceId=robert_oracle-1:ST-63657-1")
#
# })
#
# #CUTPREFIX
# test_that("Does cutPrefix cut the prefix from an entityVersionId?", {
#
# entityVersionId <- "robert_oracle-1:ST-63657-1"
# expect_equal(cutPrefix(entityVersionId) %>% unlist(), "ST-63657-1")
#
# })


