# getStepTemplate (ics1213, IMR-288)
#
# One of the thirteen exported functions the suite never entered (C9 section 7),
# and the reason it survived: it cannot work, and nothing called it.
#
# Until IMR-288 the function sourced improveR/inst/_stepTemplate.R, a file that
# the same commit which made the function public had deleted (b4faf083,
# 2025-07-18). system.file() answers a missing file with "" rather than an
# error, so every call ended in `Error: empty file/url name` - for a Step, a
# Folder, a File, an entity id, everything. Measured on 2026-09-15.
#
# The assertions below are about the template's CONTENT. A test that only
# checked is.environment() would have passed on an environment carrying nothing,
# which is exactly what the broken version would have produced had source()
# been given a file that existed but was empty.

Sys.setenv(TEST_NAME = "getStepTemplate")

setupStepTemplate <- function() {
  Sys.setenv(TEST_NAME = "getStepTemplate")
  improveR::improveConnect()
  improveR::setEditable(TRUE)

  TEST_FOLDER <- improveR:::workflowFilesSetup()
  assign("TEST_FOLDER", TEST_FOLDER, envir = globalenv())

  tree <- improveR::createAnalysisTree(
    targetIdent = TEST_FOLDER,
    treeName = paste0("gst-", format(Sys.time(), "%Y%m%d%H%M%S")),
    comment = "getStepTemplate test setup"
  )
  stopifnot("createAnalysisTree failed" = !is.null(tree))

  # Configured by value, so the template can be checked against known values
  # rather than against itself.
  env <- improveR::createStepTemplateEnv(treeIdent = tree)
  env$setStepRunserverLabel(Sys.getenv("R_RUNSERVER"))
  env$setStepToolLabel(Sys.getenv("R_TOOL"))
  env$setStepToolInstance(Sys.getenv("R_TOOL_INSTANCE"))
  env$setStepDescription("source step for getStepTemplate")
  step <- env$realise(run = FALSE)

  list(tree = tree, step = step, TEST_FOLDER = TEST_FOLDER)
}

test_that("getStepTemplate returns the source step's configuration, not an empty shell|ics1213", {
  ctx <- setupStepTemplate()
  on.exit(tryCatch(improveR::delete(ctx$tree$resourceId),
                   error = function(e) NULL), add = TRUE)

  stepId <- if (is.environment(ctx$step)) ctx$step$getStepResource()$resourceId
            else ctx$step$resourceId
  stopifnot("the fixture step was not created" = !is.null(stepId))

  tpl <- improveR::getStepTemplate(stepId)

  expect_true(is.environment(tpl))
  # The surface a template has to offer to be usable at all.
  for (fn in c("realise", "setStepDescription", "setStepRunserverLabel",
               "setStepToolLabel", "setStepToolInstance")) {
    expect_true(is.function(tpl[[fn]]),
                info = paste0("the template must carry ", fn, "()"))
  }

  # The content. These are the values the source step was configured with.
  expect_false(is.null(tpl$stepDf))
  processes <- tpl$stepDf$processes[[1]]
  expect_false(is.null(processes))
  expect_gte(nrow(processes), 1L)
  main <- processes[isTRUE(processes$main) | processes$processType == "main", , drop = FALSE]
  expect_gte(nrow(main), 1L)
  expect_equal(as.character(main$runserverLabel[1]), Sys.getenv("R_RUNSERVER"))
  expect_equal(as.character(main$toolLabel[1]), Sys.getenv("R_TOOL"))
  expect_equal(as.character(main$toolInstance[1]), Sys.getenv("R_TOOL_INSTANCE"))
})

test_that("a template taken from a step is independent of it|ics1213", {
  ctx <- setupStepTemplate()
  on.exit(tryCatch(improveR::delete(ctx$tree$resourceId),
                   error = function(e) NULL), add = TRUE)

  stepId <- if (is.environment(ctx$step)) ctx$step$getStepResource()$resourceId
            else ctx$step$resourceId

  tpl <- improveR::getStepTemplate(stepId)
  tpl$setStepDescription("changed on the template only")

  # Changing the template must not have written anything to the source step.
  source <- improveR::refreshResource(stepId)
  expect_false(identical(as.character(source$description),
                         "changed on the template only"),
               info = "a template is a copy; changing it must not write to the step")
})

test_that("getStepTemplate refuses what is not a step, by name|ics1213", {
  ctx <- setupStepTemplate()
  on.exit(tryCatch(improveR::delete(ctx$tree$resourceId),
                   error = function(e) NULL), add = TRUE)

  # Before IMR-288 every one of these ended in "empty file/url name".
  expect_error(improveR::getStepTemplate(ctx$tree$resourceId),
               regexp = "not a step", fixed = FALSE)
  expect_error(improveR::getStepTemplate(paste0(ctx$TEST_FOLDER, "/DataManipulation.R")),
               regexp = "not a step", fixed = FALSE)
})

test_that("getStepTemplate names the ident when it does not resolve|ics1213", {
  improveR::improveConnect()
  bogus <- uuid::UUIDgenerate()
  expect_error(improveR::getStepTemplate(bogus),
               regexp = "getStepTemplate", fixed = FALSE)
  expect_error(improveR::getStepTemplate(bogus),
               regexp = "does not resolve", fixed = FALSE)
  expect_error(improveR::getStepTemplate(bogus), regexp = bogus, fixed = TRUE)
})
