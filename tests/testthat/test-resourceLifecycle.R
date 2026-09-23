# Test Resource Lifecycle Functions
# Tests: finishResource, reopenResource
# Verifies finish/reopen behavior on files, folders, and inheritance.

test_that("setup resource lifecycle test", {
  tryCatch({
    improveR::improveConnect()
    improveR::setEditable(TRUE)
  }, error = function(e) {
    skip(paste("Server not available:", e$message))
  })

  basePath <- createFolderPath("resourceLifecycle")
  testFolder <- improveR::createFolder(
    targetIdent = basePath,
    folderName = paste0("test-reslc-", uniqueTag())
  )
  expect_false(is.null(testFolder))
  assign("RESLC_FOLDER", testFolder, envir = globalenv())
})

test_that("finishResource finishes a single file|ics1810", {
  skip_if(!exists("RESLC_FOLDER", envir = globalenv()), "No test folder")
  folder <- get("RESLC_FOLDER", envir = globalenv())

  testFile <- createFile(folder$resourceId, fileName = "finish-test.txt")
  expect_false(is.null(testFile))

  # Verify unfinished before
  res <- refreshResource(testFile$resourceId)
  expect_equal(res$finishedStatus, "unfinished")

  # Finish
  result <- finishResource(testFile$resourceId)
  requireServerCall(result, "finishResource")
  expect_true(result)

  # Verify finished
  res <- refreshResource(testFile$resourceId)
  expect_equal(res$finishedStatus, "finishedInherited",
               info = "File should be finished after finishResource")

  # Reopen
  result <- reopenResource(testFile$resourceId)
  expect_true(result)

  res <- refreshResource(testFile$resourceId)
  expect_equal(res$finishedStatus, "unfinished",
               info = "File should be unfinished after reopenResource")

  assign("RESLC_FILE", testFile, envir = globalenv())
})

test_that("finishResource on folder finishes all children|ics1810", {
  skip_if(!exists("RESLC_FOLDER", envir = globalenv()), "No test folder")
  folder <- get("RESLC_FOLDER", envir = globalenv())

  # Create hierarchy: folder -> childFile + subFolder -> deepFile
  childFile <- createFile(folder$resourceId, fileName = "child.txt")
  subFolder <- createFolder(folder$resourceId, "subfolder")
  deepFile <- createFile(subFolder$resourceId, fileName = "deep.txt")

  # Finish the parent folder
  result <- finishResource(folder$resourceId)
  requireServerCall(result, "finishResource")

  # All children should be finished (inherited)
  expect_equal(refreshResource(folder$resourceId)$finishedStatus, "finishedInherited")
  expect_equal(refreshResource(childFile$resourceId)$finishedStatus, "finishedInherited",
               info = "Child file should inherit finished status")
  expect_equal(refreshResource(subFolder$resourceId)$finishedStatus, "finishedInherited",
               info = "Subfolder should inherit finished status")
  expect_equal(refreshResource(deepFile$resourceId)$finishedStatus, "finishedInherited",
               info = "Deep file should inherit finished status")

  assign("RESLC_CHILD", childFile, envir = globalenv())
  assign("RESLC_SUBFOLDER", subFolder, envir = globalenv())
  assign("RESLC_DEEP", deepFile, envir = globalenv())
})

test_that("finished folder blocks creating new files|ics1810", {
  skip_if(!exists("RESLC_FOLDER", envir = globalenv()), "No test folder")
  folder <- get("RESLC_FOLDER", envir = globalenv())

  # Folder should be finished from previous test
  res <- refreshResource(folder$resourceId)
  skip_if(res$finishedStatus == "unfinished", "Folder not finished")

  # Creating a file inside should fail
  blockedFile <- createFile(folder$resourceId, fileName = "blocked.txt")
  expect_null(blockedFile, info = "Should not be able to create file in finished folder")
})

test_that("reopenResource on folder reopens all children|ics1811", {
  skip_if(!exists("RESLC_FOLDER", envir = globalenv()), "No test folder")
  folder <- get("RESLC_FOLDER", envir = globalenv())

  result <- reopenResource(folder$resourceId)
  requireServerCall(result, "reopenResource")

  # All should be unfinished
  expect_equal(refreshResource(folder$resourceId)$finishedStatus, "unfinished")

  if (exists("RESLC_CHILD", envir = globalenv())) {
    child <- get("RESLC_CHILD", envir = globalenv())
    expect_equal(refreshResource(child$resourceId)$finishedStatus, "unfinished",
                 info = "Child file should be unfinished after reopen")
  }

  if (exists("RESLC_DEEP", envir = globalenv())) {
    deep <- get("RESLC_DEEP", envir = globalenv())
    expect_equal(refreshResource(deep$resourceId)$finishedStatus, "unfinished",
                 info = "Deep file should be unfinished after reopen")
  }

  # Should be able to create files again
  newFile <- createFile(folder$resourceId, fileName = "after-reopen.txt")
  expect_false(is.null(newFile),
               info = "Should be able to create file after reopening folder")
})

test_that("finishResource returns FALSE for non-existent resource|ics1810", {
  result <- finishResource("non-existent-id-00000000")
  expect_false(result)
})

test_that("reopenResource returns FALSE for non-existent resource|ics1811", {
  result <- reopenResource("non-existent-id-00000000")
  expect_false(result)
})

test_that("cleanup resource lifecycle test", {
  if (exists("RESLC_FOLDER", envir = globalenv())) {
    folder <- get("RESLC_FOLDER", envir = globalenv())
    # Reopen first in case it's still finished
    tryCatch(reopenResource(folder$resourceId), error = function(e) {})
    tryCatch(delete(folder$resourceId), error = function(e) {
      cat("Cleanup warning:", e$message, "\n")
    })
  }
  for (v in c("RESLC_FOLDER", "RESLC_FILE", "RESLC_CHILD", "RESLC_SUBFOLDER", "RESLC_DEEP")) {
    if (exists(v, envir = globalenv())) rm(list = v, envir = globalenv())
  }
  expect_true(TRUE)
})
