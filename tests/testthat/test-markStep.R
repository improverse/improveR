# Test markStep — set/clear boolean flags on live steps

Sys.setenv(TEST_NAME = "markStep")

ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME = "markStep")
  if (!exists("TEST_FOLDER") || is.null(TEST_FOLDER)) {
    improveR::setEditable(TRUE)
    TEST_FOLDER <- improveR:::workflowFilesSetup()
    assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
  }
  return(get("TEST_FOLDER", envir = globalenv()))
}

test_that("setup markStep test", {
  TEST_FOLDER <- ensureTestFolder()
  testTree <- improveR::createAnalysisTree(
    targetIdent = TEST_FOLDER,
    treeName = paste0("MarkStepTest-", format(Sys.time(), "%H%M%S"))
  )
  assign("MS_TREE", testTree, envir = globalenv())

  stepEnv <- createStepTemplateEnv(treeIdent = testTree)
  stepEnv$setStepRunserverLabel(Sys.getenv("R_RUNSERVER"))
  stepEnv$setStepToolLabel(Sys.getenv("R_TOOL"))
  stepEnv$setStepToolInstance(Sys.getenv("R_TOOL_INSTANCE"))
  stepEnv$setStepDescription("Flag test step")
  stepEnv$setStepRationale("Testing markStep")
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"),
                            variableName = "command-file")
  realised <- stepEnv$realise(run = FALSE)
  stepResource <- realised$getStepResource()
  assign("MS_STEP", stepResource, envir = globalenv())
  cat("Created step:", stepResource$path, "\n")
})

test_that("template setStepFinalModel sets flag at creation time", {
  TEST_FOLDER <- ensureTestFolder()
  testTree <- get("MS_TREE", envir = globalenv())

  stepEnv <- createStepTemplateEnv(treeIdent = testTree)
  stepEnv$setStepRunserverLabel(Sys.getenv("R_RUNSERVER"))
  stepEnv$setStepToolLabel(Sys.getenv("R_TOOL"))
  stepEnv$setStepToolInstance(Sys.getenv("R_TOOL_INSTANCE"))
  stepEnv$setStepDescription("Pre-flagged step")
  stepEnv$setStepRationale("Testing template flags")
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"),
                            variableName = "command-file")
  stepEnv$setStepFinalModel(TRUE)
  stepEnv$setStepKeyStep(TRUE)

  realised <- stepEnv$realise(run = FALSE)
  stepRes <- realised$getStepResource()
  assign("MS_STEP_PREFLAG", stepRes, envir = globalenv())

  step <- refreshResource(stepRes)
  expect_true(step$finalModel, info = "finalModel should be TRUE from template")
  expect_true(step$keyStep, info = "keyStep should be TRUE from template")
  expect_false(step$baseModel, info = "baseModel should default to FALSE")
  cat("Pre-flagged step:", step$path, "finalModel:", step$finalModel, "keyStep:", step$keyStep, "\n")
})

test_that("markStep sets flags on existing step", {
  skip_if(!exists("MS_STEP", envir = globalenv()), "No step created")
  stepResource <- get("MS_STEP", envir = globalenv())

  # Initially all flags should be FALSE
  step <- refreshResource(stepResource)
  expect_false(step$keyStep, info = "keyStep should start FALSE")
  expect_false(step$finalModel, info = "finalModel should start FALSE")

  # Set finalModel
  result <- markStep(stepResource$resourceId, finalModel = TRUE)
  expect_false(is.null(result))
  expect_true(result$finalModel, info = "finalModel should be TRUE after markStep")
  expect_false(result$keyStep, info = "keyStep should remain FALSE")
  cat("After markStep(finalModel=TRUE):", result$finalModel, "\n")
})

test_that("markStep sets multiple flags at once", {
  skip_if(!exists("MS_STEP", envir = globalenv()), "No step created")
  stepResource <- get("MS_STEP", envir = globalenv())

  result <- markStep(stepResource$resourceId,
                     keyStep = TRUE, baseModel = TRUE, referenceModel = TRUE)
  expect_true(result$keyStep)
  expect_true(result$baseModel)
  expect_true(result$referenceModel)
  # finalModel was set in previous test — should still be TRUE
  expect_true(result$finalModel, info = "finalModel should survive setting other flags")
  cat("Multiple flags set:", result$keyStep, result$baseModel, result$referenceModel, "\n")
})

test_that("markStep clears flags", {
  skip_if(!exists("MS_STEP", envir = globalenv()), "No step created")
  stepResource <- get("MS_STEP", envir = globalenv())

  result <- markStep(stepResource$resourceId, finalModel = FALSE, baseModel = FALSE)
  expect_false(result$finalModel, info = "finalModel should be cleared")
  expect_false(result$baseModel, info = "baseModel should be cleared")
  # keyStep was NOT passed — should still be TRUE from previous test
  expect_true(result$keyStep, info = "keyStep should survive clearing other flags")
  cat("After clearing:", "finalModel:", result$finalModel, "keyStep:", result$keyStep, "\n")
})

test_that("markStep with no flags is a no-op", {
  skip_if(!exists("MS_STEP", envir = globalenv()), "No step created")
  stepResource <- get("MS_STEP", envir = globalenv())

  before <- refreshResource(stepResource)
  result <- markStep(stepResource$resourceId)
  after <- refreshResource(stepResource)
  expect_equal(before$keyStep, after$keyStep)
  expect_equal(before$finalModel, after$finalModel)
})

test_that("cleanup markStep test", {
  if (exists("MS_TREE", envir = globalenv())) {
    tryCatch(improveR::delete(get("MS_TREE", envir = globalenv())$resourceId),
             error = function(e) NULL)
  }
  for (v in c("MS_TREE", "MS_STEP", "MS_STEP_PREFLAG")) {
    if (exists(v, envir = globalenv())) rm(list = v, envir = globalenv())
  }
  expect_true(TRUE)
})
