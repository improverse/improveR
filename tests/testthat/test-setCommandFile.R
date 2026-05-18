# Test setCommandFile + getProcessFileVariables + createProcessFileVariable
#
# Regression test for the process-variable list parser. Previously the
# helpers in step.R used
#   plyr::rbind.fill(lapply(httr::content(result), as.data.frame))
# which silently misinterpreted a flat single-object POST response (the
# shape returned when creating ONE variable) as 5 separate one-cell
# rows, losing the API field names. Downstream subsetting
#   newVars[newVars$name == "command-file", ]
# then produced a value whose nrow() was NULL, so
#   if (nrow(cmdFileRow) == 0) ...
# raised "argument is of length zero" — uncatchable in the sense that
# callers got no useful error class and the dialog observer body died
# past removeModal().
#
# These tests assert the post-fix contract: both functions return a
# proper data frame with the API's named columns (id, name,
# variableType, position, valueResourceId, processId), one row per
# variable. setCommandFile completes end-to-end on a freshly created
# step without raising.

Sys.setenv(TEST_NAME = "setCommandFile")

ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME = "setCommandFile")
  if (!exists("TEST_FOLDER") || is.null(TEST_FOLDER)) {
    improveR::setEditable(TRUE)
    TEST_FOLDER <- improveR:::workflowFilesSetup()
    assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
  }
  return(get("TEST_FOLDER", envir = globalenv()))
}

# Build a fresh step + main process under TEST_FOLDER. Returns the step
# resource df with $resourceId / $entityId, plus the main process row.
createFreshStep <- function(suffix) {
  TEST_FOLDER <- ensureTestFolder()
  treeName <- paste0("SCF-", suffix, "-", format(Sys.time(), "%H%M%S"))
  tree <- improveR::createAnalysisTree(
    targetIdent = TEST_FOLDER,
    treeName = treeName
  )
  stepEnv <- improveR::createStepTemplateEnv(treeIdent = tree)
  stepEnv$setStepRunserverLabel(Sys.getenv("R_RUNSERVER"))
  stepEnv$setStepToolLabel(Sys.getenv("R_TOOL"))
  stepEnv$setStepToolInstance(Sys.getenv("R_TOOL_INSTANCE"))
  stepEnv$setStepDescription(paste("setCommandFile test:", suffix))
  stepEnv$setStepRationale("variable parser regression test")
  realised <- stepEnv$realise(run = FALSE)
  step <- realised$getStepResource()
  mp <- improveR:::getMainProcess(step)
  list(step = step, mainProcess = mp, processId = as.character(mp$id))
}


test_that("getProcessFileVariables on a fresh step returns an empty data frame (or NULL), not a malformed shape", {
  env <- createFreshStep("empty")
  vars <- improveR::getProcessFileVariables(env$step, env$processId)

  # Tolerate either an empty data frame or NULL — callers handle both,
  # and the contract is "no variables = nothing to iterate". What MUST
  # NOT happen is a malformed multi-row, one-column 'X[[i]]' frame.
  if (!is.null(vars)) {
    expect_s3_class(vars, "data.frame")
    if (nrow(vars) > 0) {
      expect_true(all(c("id", "name") %in% names(vars)))
    }
    expect_false("X[[i]]" %in% names(vars))
  }
})


test_that("createProcessFileVariable returns a 1-row data frame with API field names as columns", {
  env <- createFreshStep("create")
  new_var <- improveR:::createProcessFileVariable(
    env$step, env$processId,
    name = "command-file",
    variableType = "fileRef",
    position = 0
  )

  expect_s3_class(new_var, "data.frame")
  expect_equal(nrow(new_var), 1L)
  # Must NOT be the pre-fix shape (5 rows, single 'X[[i]]' column).
  expect_false("X[[i]]" %in% names(new_var))
  # Must expose the API field names as columns.
  expect_true(all(c("id", "name", "variableType", "position") %in% names(new_var)))
  expect_equal(new_var$name, "command-file")
  expect_equal(new_var$variableType, "fileRef")
  expect_true(nzchar(new_var$id))
})


test_that("getProcessFileVariables after a variable exists returns it with named columns", {
  env <- createFreshStep("get-after-create")
  improveR:::createProcessFileVariable(
    env$step, env$processId,
    name = "command-file", variableType = "fileRef", position = 0
  )
  vars <- improveR::getProcessFileVariables(env$step, env$processId)

  expect_s3_class(vars, "data.frame")
  expect_gte(nrow(vars), 1L)
  expect_true("name" %in% names(vars))
  expect_true("command-file" %in% vars$name)
})


test_that("setCommandFile on a fresh step completes without raising and binds the file", {
  TEST_FOLDER <- ensureTestFolder()
  env <- createFreshStep("setcmd")

  # Upload a tiny R script as a step input file.
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("# command body", tmp)
  fileRes <- improveR::createFile(env$step, fileName = "test_cmd.R",
                                  localPath = tmp)
  expect_false(is.null(fileRes))
  expect_true(nzchar(fileRes$entityId))

  # This is the call that previously raised "argument is of length zero"
  # with no condition class.
  result <- improveR::setCommandFile(env$step$entityId, fileRes$entityId)

  expect_s3_class(result, "data.frame")
  expect_gte(nrow(result), 1L)
  expect_true("name" %in% names(result))
  expect_true("command-file" %in% result$name)

  # The command-file row must reference the file we just uploaded.
  cmdRow <- result[result$name == "command-file", , drop = FALSE]
  expect_equal(nrow(cmdRow), 1L)
  if ("valueResourceId" %in% names(cmdRow)) {
    expect_equal(cmdRow$valueResourceId[1], fileRes$resourceId)
  }
})


test_that("setCommandFile is idempotent — calling it twice doesn't duplicate the variable", {
  env <- createFreshStep("idempotent")

  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("# c", tmp)
  file1 <- improveR::createFile(env$step, fileName = "cmd1.R", localPath = tmp)
  result1 <- improveR::setCommandFile(env$step$entityId, file1$entityId)
  expect_s3_class(result1, "data.frame")

  file2 <- improveR::createFile(env$step, fileName = "cmd2.R", localPath = tmp)
  result2 <- improveR::setCommandFile(env$step$entityId, file2$entityId)
  expect_s3_class(result2, "data.frame")

  # The variable list should still contain exactly ONE command-file row;
  # the second setCommandFile must rebind, not create a duplicate.
  cmdRows <- result2[result2$name == "command-file", , drop = FALSE]
  expect_equal(nrow(cmdRows), 1L)
})
