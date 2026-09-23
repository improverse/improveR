Sys.setenv(TEST_NAME="steps")

# Helper function to ensure TEST_FOLDER exists when running tests individually
ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME="steps")
  if (!exists("TEST_FOLDER") || is.null(TEST_FOLDER)) {
    improveR::setEditable(TRUE)
    TEST_FOLDER <- improveR:::workflowFilesSetup()
    assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
    return(TEST_FOLDER)
  }
  return(TEST_FOLDER)
}

rBatchStep <- function(testTree) {
  r_runserver <- Sys.getenv("R_RUNSERVER")
  r_tool <- Sys.getenv("R_TOOL")
  r_tool_instance <- Sys.getenv("R_TOOL_INSTANCE")

  stepEnv <- createStepTemplateEnv(treeIdent=testTree)
  stepEnv$setStepRunserverLabel(r_runserver)
  stepEnv$setStepToolLabel(r_tool)
  stepEnv$setStepToolInstance(r_tool_instance)

  return(stepEnv)
}

mockStep <- function(tree, dataSet, name, description, dataSet2 = NULL, dataSet3 = NULL) {
  stepEnv <- rBatchStep(tree)
  stepEnv$setStepDescription(name)
  stepEnv$setStepRationale(description)
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"), variableName = "command-file")
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.Rmd"))
  stepEnv$addStepRemoteFile(dataSet, name = "data.csv")

  if (!is.null(dataSet2)) {
    stepEnv$addStepRemoteFile(dataSet2, name = "data2.csv")
  }
  if (!is.null(dataSet3)) {
    stepEnv$addStepRemoteFile( dataSet3, name = "data3.csv")
  }
  realStep <- stepEnv$realise(run=F)

  # inventory <- realStep$getStepInventory()$data[[1]]
  # dataSet <- inventory %>%
  #   dplyr::filter(name == "chapter15_example_cleaned.rds") %>%
  #   dplyr::select("entityId") %>% as.character()
  # return(dataSet)
}


test_that("test for attaching step to itself|ics1225,ics2046,ics1205", {
  TEST_FOLDER <- ensureTestFolder()

  testTree <- improveR::createAnalysisTree(
    targetIdent = TEST_FOLDER,
    treeName = "stepsTree"
  )

  stepCreated <- createStep(treeIdent = testTree)

  #attach step to itself
  stepTestAttached <- attachStep(
    ident = stepCreated$resourceId,
    parent = stepCreated$resourceId
  )

  #should not have a child resource which has the same ident as itself
  stepChildrenIds <- loadChildSteps(
    ident = stepTestAttached$resourceId
  )$data[[1]]$entityId
  expect_false(stepCreated$entityId %in% stepChildrenIds)

  #resource cannot be its own parent
  expect_false(stepTestAttached$resourceId == stepTestAttached$parentId)

  #step remains unchanged after trying to attach it to itself
  expect_true(identical(stepCreated, stepTestAttached))
})
# --- Step description and rationale (IMR-280) ------------------------------
#
# changeStepDescription and changeStepRationale were among the exported
# functions the suite never entered (run 25, 2026-09-14). Both are built the
# same way: load the resource, set one field, PUT the whole entity back, and
# return refreshResource().
#
# Neither checks what the PUT returned:
#
#     result <- authenticatedREST(..., restType = "PUT")
#     return(refreshResource(ident, from))
#
# `result` is assigned and never read. A failed PUT therefore yields the
# UNCHANGED resource and the caller sees a success - the same defect family as
# IMR-270, IMR-271 and IMR-275. Asserting the text that comes back, rather
# than that a resource came back, is what catches it.

test_that("changeStepDescription writes the description and the change is visible|ics1217", {
  TEST_FOLDER <- ensureTestFolder()
  testTree <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER,
                                           treeName = "stepTextTree")
  step <- improveR:::createStep(treeIdent = testTree)
  expect_false(is.null(step), info = "the fixture step must be created")

  first <- paste0("imr280 description ", uuid::UUIDgenerate())
  changed <- improveR::changeStepDescription(step$resourceId, description = first)
  expect_false(is.null(changed))
  expect_equal(changed$description, first,
               info = "the returned resource must carry the description that was set")

  # Read it back independently of the return value - a function that returns
  # its own input would pass the check above.
  reloaded <- improveR::loadResource(step$resourceId)
  expect_equal(reloaded$description, first,
               info = "the description must be on the server, not only in the return value")

  # A second, different value. One set-and-read says nothing about whether the
  # second call works: a no-op that returns the resource passes it once.
  second <- paste0("imr280 description second ", uuid::UUIDgenerate())
  improveR::changeStepDescription(step$resourceId, description = second)
  expect_equal(improveR::loadResource(step$resourceId)$description, second,
               info = "a second change must replace the first")
})

test_that("changeStepRationale writes the rationale and leaves the description alone|ics1217", {
  TEST_FOLDER <- ensureTestFolder()
  testTree <- improveR::createAnalysisTree(targetIdent = TEST_FOLDER,
                                           treeName = "stepRationaleTree")
  step <- improveR:::createStep(treeIdent = testTree)
  expect_false(is.null(step), info = "the fixture step must be created")

  description <- paste0("imr280 keep ", uuid::UUIDgenerate())
  improveR::changeStepDescription(step$resourceId, description = description)

  rationale <- paste0("imr280 rationale ", uuid::UUIDgenerate())
  changed <- improveR::changeStepRationale(step$resourceId, rationale = rationale)
  expect_false(is.null(changed))
  expect_equal(changed$rationale, rationale,
               info = "the returned resource must carry the rationale that was set")

  # Both functions PUT the WHOLE entity back, so the one that is not being
  # changed is the one at risk. This is the assertion that would catch it.
  reloaded <- improveR::loadResource(step$resourceId)
  expect_equal(reloaded$rationale, rationale)
  expect_equal(reloaded$description, description,
               info = paste0("changeStepRationale must not disturb the description - ",
                             "both functions PUT the entire entity"))
})
