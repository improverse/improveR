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


test_that("improveConnected handles uninitialized state correctly", {

  if (exists("cacheEnv")) {
    
    assign("initialized", FALSE, envir = cacheEnv)

  }
  # expect_false(improveConnected(silent=TRUE))
  expect_false(improveConnected())

})


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
test_that("OLD VERSION. improveClose handles file cleanup correctly", {
  
  improveR:::setRootPath(getwd())

  test_dir <- file.path(tempdir(), "test_improveClose")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE)) #when exiting function remove folder and nested folders
  
  # Write test files to temp folder
  pathFile1 <- file.path(test_dir, "file1.txt")
  pathFile2 <- file.path(test_dir, "file2.txt")
  pathWithoutFile <- file.path(test_dir, "Withoutfile.txt")
  writeLines(text="test1", con=pathFile1) #store files with some content in the temp folder
  writeLines(text="test2", con=pathFile2)
 
  # assign package cacheEnv to the environment of test function
  cacheEnv <- improveR:::cacheEnv

  cacheEnv$createdLinks <- data.frame(
    localPath = c(pathFile1, pathFile2, pathWithoutFile), 
    type=c("Link", "Link", "Link"),
    path=c(basename(pathFile1), basename(pathFile2), basename(pathWithoutFile)),
    target=c("res1","res2","res3"),
    stringsAsFactors = FALSE
  )

  # Test before calling improveClose / files should exist
  expect_true(exists("createdLinks", envir=cacheEnv))
  expect_true("localPath" %in% names(cacheEnv$createdLinks))
  expect_true(file.exists(pathFile1))
  expect_true(file.exists(pathFile2))
  
  #adding this test shows that we are actually testing someting different
  #we are testing a "copy" of the cacheEnv which we created when starting improveR;
  #we are not testing bindings inside of improveR::cacheEnv; 
  #but that's what the test should actually do
  #expect_true(exists("createdLinks", envir=improveR::cacheEnv)) # TODO add test again later

  improveR::improveClose()
    
  expect_true(exists("cacheEnv"))
  expect_true(is.environment(cacheEnv))
  expect_true(exists("createdLinks", envir=cacheEnv))


  # files should be removed
  expect_false(file.exists(pathFile1))
  expect_false(file.exists(pathFile2))

})

# NOTE # @HACKLM
# same test as above but with out cacheEnv assignment
# as it turns out the initial problem was not about accessing the improveR::cacheEnv
# in the tests, but correctly accessing/specifiyng the paths in the test since
# saveImproveJson also requires path and target (not only pathLocal)
# the tried approach cacheEnv<-improveR:::cacheEnv is not necessary and in fact is testing
# something different than intended; it tests the presnece of specific bindings in a "copy" of improveR:::cacheEnv 
# and not in improveR::cacheEnv itself. The approach below takes care of this
test_that("ALTERNTAIVE - improveClose handles file cleanup correctly", {
  
  # improveR:::setRootPath(getwd()) #not needed

  test_dir <- file.path(tempdir(), "test_improveClose")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE)) #when exiting function remove folder and nested folders
  
  # Write test files to temp folder
  pathFile1 <- file.path(test_dir, "file1.txt")
  pathFile2 <- file.path(test_dir, "file2.txt")
  pathWithoutFile <- file.path(test_dir, "Withoutfile.txt")
  writeLines(text="test1", con=pathFile1) #store files with some content in the temp folder
  writeLines(text="test2", con=pathFile2)
 
  # assign package cacheEnv to the environment of test function
  # cacheEnv <- improveR:::cacheEnv #REMOVED

  cacheEnv$createdLinks <- data.frame(
    localPath = c(pathFile1, pathFile2, pathWithoutFile), 
    fileName=c(basename(pathFile1), basename(pathFile2), basename(pathWithoutFile)),
    path = c(pathFile1, pathFile2, pathWithoutFile), 
    target=c("res1","res2","res3"),
    stringsAsFactors = FALSE
  )

  # Test before calling improveClose / files should exist
  expect_true(exists("createdLinks", envir=cacheEnv))
  expect_true("localPath" %in% names(cacheEnv$createdLinks))
  expect_true(file.exists(pathFile1))
  expect_true(file.exists(pathFile2))

  improveR::improveClose()

  # Tests after calling improveClose  
  expect_true(exists("cacheEnv"))
  expect_true(is.environment(cacheEnv))
  expect_true(exists("createdLinks", envir=cacheEnv))

  # files should be removed
  expect_false(file.exists(pathFile1))
  expect_false(file.exists(pathFile2))

})
