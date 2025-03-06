#assign environment of package to environment used during the test
# cacheEnv <- improveR::cacheEnv
#  improveR::cacheEnv <- cacheEnv

#improveConnected
# test_that("improveConnected fails when not initialized", {
#   # Start test with empty cacheEnv environment
#   if (exists("cacheEnv")) {
#     rm(list = ls(envir = cacheEnv), envir = cacheEnv)
#   }
  
#   # Test uninitialized state
#   expect_message(improveConnected(), regexp="No connection detected")
#   #expect_error(improveConnected(), regexp="was not called")
  
#   # Test error message contains expected text
#   # expect_message(
#   #   improveConnected(), 
#   #   # regexp = "not connected"
#   #   regexp = "was not called"
#   # )
# })

#TODO
# test_that("improveConnected handles uninitialized state correctly", {

#   if (exists("cacheEnv")) {
#     # rm(list = ls(envir = cacheEnv), envir = cacheEnv)
#     assign("initialized", NULL, envir = cacheEnv)

#   }
#   # expect_false(improveConnected(silent=TRUE))
#   expect_false(improveConnected())

# })


test_that("improveConnected succeeds when properly initialized", {
  
  if (exists("cacheEnv")) {
  cacheEnv$initialized <- TRUE
  }

  # Test: should return true
  expect_true(improveConnected())

  # Cleanup
  rm(list = ls(envir = cacheEnv), envir = cacheEnv)
})


#improveDisconnect
test_that("improveDisconnect removes all connection information from the environment", {
  
  #set up environment with connection information
  cacheEnv <- new.env(parent=emptyenv()) #empty environment as a parent

  #populate envir with dummy bindings
  cacheEnv$initialized <- TRUE
  cacheEnv$logLevel <- "INFO"
  cacheEnv$secure <- "secure"
  cacheEnv$offlinePossible <- FALSE
  cacheEnv$offline <- FALSE
  cacheEnv$persistentCaching <- FALSE
  cacheEnv$reproducible <- FALSE
  cacheEnv$reqToken <- "token"
  cacheEnv$stepId <- "stepId"
  cacheEnv$repoUrl <- "repoURL"
  cacheEnv$user <- "userName"
  cacheEnv$password <- "password"

 improveR::improveDisconnect(env=cacheEnv)
 expect_equal(length(ls(cacheEnv)), 0)

}
)

#improveClose
## NOTE
### -also includes instance where a path to a file is still in env, but no corresponding file exists
### -currently only test the deletion process
## NOTE
### - challenge was to make cacheEnv refer to the same object; tests run 
###   in their own environment; calling improveClose without a function argument
###   triggers a scoping issue; introducing cacheEnv as an explicit argument in the
###   the definiton of improveClose solves the issue. 
###IMPROVEMENT maybe improveClose(env=cachEnv); set it as a default?

test_that("improveClose handles file cleanup correctly", {
  
  # Create temporary folder
  test_dir <- file.path(tempdir(), "test_improveClose")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE)) #when exiting function remove folder and nested folders
  
  # Write test files to temp folder
  path_file1 <- file.path(test_dir, "test1.txt")
  path_file2 <- file.path(test_dir, "test2.txt")
  path_file_no_file <- file.path(test_dir, "test_no_file.txt")
  writeLines(text="test1", con=path_file1) #store files with some content in the temp folder
  writeLines(text="test2", con=path_file2)
  
  # Create environment with references to temp test files
  # Checks also case where env contains link to non-exisitng file
  cacheEnv <- new.env(parent=emptyenv())
  cacheEnv$createdLinks <- data.frame(
    localPath = c(path_file1, path_file2, path_file_no_file), 
    info=c("info on file 1", "info on file 2", NA),
    stringsAsFactors = FALSE
  )
  # Mock response from saveImproveJson; but saveImproveJson doesn't return anything
  # Should it be actually tested here or does it suffice to test only saveImproveJson?
  json_saved <- FALSE
  local_mocked_bindings(
    saveImproveJson = function(...) {json_saved <<- TRUE},
    .env=asNamespace("improveR")
  )
  
  # Run function
  # improveClose(cache=cacheEnv)
  improveClose(cacheEnv)
  
  # Test
  expect_false(file.exists(path_file1))
  expect_false(file.exists(path_file2))
  expect_true(json_saved)

})
