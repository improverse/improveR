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

# REMOVE dummy test to clarify cacheEnv issue
test_that("Does cacheEnv exist?", {
  expect_true(exists("cacheEnv"))
})  


#improveClose
# TODO # QUESTION # @HACKLM 
## PROBLEM: test below creates files in a temp folder; their location is assigned to the new
## environment cacheEnv; improveClose is called to delete the files; the test checks
## if the files are deleted; test however fails;
## Running the test file and improveClose stepwise/interactively works as intended;
## there seems to be again an issue of non-matching environments; 
##
## UNDERSTANDING SO FAR:
## - each test is executed in its own environnment (no interference between different tests)
## - but they are executed in the parent environment created by the package (which is loaded in testthat.R - library(improveR), what sets up cacheEnv even before
##  calling improveConnect(); this is confirmed by a "dummy test" (Does cacheEnv exist?, see above). It confirms the existance of cacheEnv, also when running the test
##  non-interactively/from a clean slate. This suggests that the modification to cacheEnv stipulated in the test "improveClose handles file cleanup correctly" (below) 
##  does not write to the "same" cacheEnv as created by the package.
##
## APPROACHES TRIED/NARROWING DOWN THE ISSUE
## - using explicitly improveR::cacheEnv in test file results in Error in `eval(code, test_env)`: object 'improveR' not found
## - adding on top of test file: cacheEnv <- improveR::cacheEnv; Error: 'cacheEnv' is not an exported object from 'namespace:improveR'
## - created the dummy functions "dummyAddElementToCache" and the pertaining test file "test-dummyAddElementToCache" (see files included temporarily (!) into the package). 
##   It demonstrates that adding an element to cachEnv inside a test works as intended. The function dummyAddElementToCache adds an element to cacheEnv during the test; the
##   test confirms the existence of "new element" inside cacheEnv; => this suggests the mismatch of environments is specific to test of improveClose;
##   Note that dummyAddElementToCache works even thouch cacheEnv is not passed as an argument;
## - added expect_true(file.exists(path_file1)) etc before calling improveClose(): tests pass; confirms that the files exist;
## - wrapping entrie test_that body in  
##      chacheEnv <- environment(improveClose)
##      with(cacheEnv, { })
##   resulted in error: cannot add bindings to locked environment
## - environment(improveClose) <-  environment(): tryinng to assign test environment as environment of function; tests still fail
##
## SOLUTION SO FAR
## - make cacheEnv an explict argument of improveClose: So far the only solution 
##   was to include cacheEnv as an explicit argument in the definiton of improveClose,
##   but this is an approach - as briefly discussed - we want to avoid (e.g. repercussions on other functions)
# test_that("improveClose handles file cleanup correctly", {
  
# # cacheEnv <- environment(improveClose)

# # with(cacheEnv, {

#   # Create temporary folder
#   test_dir <- file.path(tempdir(), "test_improveClose")
#   dir.create(test_dir, showWarnings = FALSE)
#   on.exit(unlink(test_dir, recursive = TRUE)) #when exiting function remove folder and nested folders
  
#   # Write test files to temp folder
#   path_file1 <- file.path(test_dir, "test1.txt")
#   path_file2 <- file.path(test_dir, "test2.txt")
#   path_file_no_file <- file.path(test_dir, "test_no_file.txt")
#   writeLines(text="test1", con=path_file1) #store files with some content in the temp folder
#   writeLines(text="test2", con=path_file2)
 
#   # Create environment with references to temp test files
#   # Checks also case where env contains link to non-exisitng file
#   cacheEnv <- new.env(parent=emptyenv())
#   cacheEnv$createdLinks <- data.frame(
#     localPath = c(path_file1, path_file2, path_file_no_file), 
#     info=c("info on file 1", "info on file 2", NA),
#     stringsAsFactors = FALSE
#   )

#   # Test before calling improveClose / files should exist
#   expect_true(exists("createdLinks", envir=cacheEnv))
#   expect_true("localPath" %in% names(cacheEnv$createdLinks))
#   expect_true(file.exists(path_file1))
#   expect_true(file.exists(path_file2))

#   # Run function
#   # environment(improveClose) <-  environment()
#   improveR::improveClose()
    
#   # Test (all passing)
#   expect_true(exists("cacheEnv"))
#   expect_true(is.environment(cacheEnv))
#   expect_true(exists("createdLinks", envir=cacheEnv))
  
#   # files should be removed
#   expect_false(file.exists(path_file1))
#   expect_false(file.exists(path_file2))
#   # # expect_true(json_saved)
# # })

# })
