#convertDate






# test_that("loadResourceFromServer loads resource", {

#  # Mock setup
#   # mock_response_authenticatedREST <- list(
#   #   entityId = "robert_oracle-1:ST-63678",
#   #   entityVersionId = "robert_oracle-1:ST-63678-1",
#   #   lastModifiedOn = "1677667200000",  # Example timestamp
#   #   resourceId = "B38A9D2BAE764B879CE7E54270087673"
#   # )

#   local_mocked_bindings(
#     improveConnected = function() TRUE,
#     authenticatedREST = function(...) {
#       structure(
#         list(content = mock_response_authenticatedREST),
#         class = "response"
#       )
#     },
#     convertAPIListToDataframe = function(x) as.data.frame(x, stringsAsFactors = FALSE),
#     repoPrefix = function() "test:",
#     httr::content = function(x) x$content
#   )

# #fn execution
# loadResourceFromServer(resourceId="robert_oracle-1:ST-63678")

# })

test_that("Does cacheEnv exist", {
  expect_true(exists("cacheEnv"))
})


test_that("Does cacheEnv include pwd?", {
  #  expect_equal(cacheEnv$pwd$entityId, "robert_oracle-1:ST-63678")
  improveConnect()
  expect_true(rlang::env_has(cacheEnv, "pwd")) #
})

