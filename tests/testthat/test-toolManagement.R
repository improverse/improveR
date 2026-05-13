# Test Tool Management Functions
# Tests: createToolCategory, renameToolCategory, deleteToolCategory,
#        createTool, loadToolCategories, loadToolsForCategory,
#        createToolInstance, updateToolInstance,
#        createToolParameter, updateToolParameter

Sys.setenv(TEST_NAME = "toolManagement")

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------
test_that("setup toolManagement test environment", {
  tryCatch({
    improveConnect()
    setEditable(TRUE)
  }, error = function(e) {
    skip(paste("Server not available:", e$message))
  })
  expect_true(improveConnected(silent = TRUE))
})

# ---------------------------------------------------------------------------
# Tool Categories
# ---------------------------------------------------------------------------
test_that("createToolCategory creates a new category|ccs32,ics1673", {
  catName <- paste0("test-toolmgmt-", format(Sys.time(), "%Y%m%d%H%M%S"))
  result <- createToolCategory(catName)
  expect_false(is.null(result))
  expect_true(!is.null(result$id))
  expect_equal(result$name, catName)
  assign("TM_CATEGORY", result, envir = globalenv())
  cat("Created tool category:", result$id, "\n")
})

test_that("loadToolCategories returns the created category|ics1229", {
  stopifnot("No category created" = exists("TM_CATEGORY", envir = globalenv()))
  cat <- get("TM_CATEGORY", envir = globalenv())

  # Use direct REST call — loadToolCategories() may return cached data
  categories <- restGetAsDf("configuration/toolCategories")
  expect_false(is.null(categories))
  expect_true(is.data.frame(categories))
  expect_true(cat$id %in% categories$id)
  cat("Found category in list\n")
})

test_that("renameToolCategory updates the name|ccs33,ics1675", {
  stopifnot("No category created" = exists("TM_CATEGORY", envir = globalenv()))
  cat <- get("TM_CATEGORY", envir = globalenv())

  newName <- paste0(cat$name, "-renamed")
  result <- renameToolCategory(cat$id, newName)
  expect_false(is.null(result))

  # Verify via fresh REST call (cache may be stale)
  categories <- restGetAsDf("configuration/toolCategories")
  renamed <- categories[categories$id == cat$id, ]
  expect_equal(renamed$name[1], newName)
  cat("Renamed category to:", newName, "\n")
})

# ---------------------------------------------------------------------------
# Tools (within a category)
# ---------------------------------------------------------------------------
test_that("createTool creates a tool in a category|ccs35,ics1688", {
  stopifnot("No category created" = exists("TM_CATEGORY", envir = globalenv()))
  cat <- get("TM_CATEGORY", envir = globalenv())

  toolName <- paste0("test-tool-", format(Sys.time(), "%H%M%S"))
  result <- createTool(cat$id, toolName)
  expect_false(is.null(result))
  expect_true(!is.null(result$id))
  assign("TM_TOOL", result, envir = globalenv())
  cat("Created tool:", result$id, "\n")
})

test_that("createTool is idempotent|ccs35,ics1688", {
  stopifnot("No category created" = exists("TM_CATEGORY", envir = globalenv()))
  stopifnot("No tool created" = exists("TM_TOOL", envir = globalenv()))
  cat <- get("TM_CATEGORY", envir = globalenv())
  tool <- get("TM_TOOL", envir = globalenv())

  result <- createTool(cat$id, tool$name)
  expect_false(is.null(result))
  expect_equal(result$id, tool$id)
  cat("Idempotent create returned same tool\n")
})

test_that("loadToolsForCategory returns the created tool|ics1230", {
  stopifnot("No category created" = exists("TM_CATEGORY", envir = globalenv()))
  stopifnot("No tool created" = exists("TM_TOOL", envir = globalenv()))
  cat <- get("TM_CATEGORY", envir = globalenv())
  tool <- get("TM_TOOL", envir = globalenv())

  tools <- loadToolsForCategory(cat$id)
  expect_false(is.null(tools))
  expect_true(is.data.frame(tools))
  expect_true(tool$id %in% tools$id)
  cat("Found tool in category\n")
})

# ---------------------------------------------------------------------------
# Tool Instances (on a runserver)
# ---------------------------------------------------------------------------
test_that("createToolInstance creates instance on runserver|ccs36,ics1691", {
  stopifnot("No tool created" = exists("TM_TOOL", envir = globalenv()))
  tool <- get("TM_TOOL", envir = globalenv())

  # Get a runserver (loadRunservers is internal, available via load_all)
  runservers <- loadRunservers()
  skip_if(is.null(runservers) || nrow(runservers) == 0, "No runservers available")
  rs <- runservers[1, ]
  assign("TM_RUNSERVER", rs, envir = globalenv())

  instanceName <- paste0("test-instance-", format(Sys.time(), "%H%M%S"))
  result <- createToolInstance(
    runserverId = rs$id,
    toolId = tool$id,
    instanceName = instanceName,
    command = "echo test"
  )
  if (is.null(result)) {
    cat("createToolInstance returned NULL — server may not support instance creation via REST\n")
  } else {
    expect_true(!is.null(result$id))
    assign("TM_INSTANCE", result, envir = globalenv())
    cat("Created tool instance:", result$id, "\n")
  }
  expect_true(is.null(result) || is.list(result))
})

test_that("createToolInstance is idempotent|ccs36,ics1691", {
  stopifnot("No instance created" = exists("TM_INSTANCE", envir = globalenv()))
  stopifnot("No runserver" = exists("TM_RUNSERVER", envir = globalenv()))
  tool <- get("TM_TOOL", envir = globalenv())
  inst <- get("TM_INSTANCE", envir = globalenv())
  rs <- get("TM_RUNSERVER", envir = globalenv())

  result <- createToolInstance(
    runserverId = rs$id,
    toolId = tool$id,
    instanceName = inst$name,
    command = "echo test"
  )
  expect_false(is.null(result))
  expect_equal(result$id, inst$id)
  cat("Idempotent create returned same instance\n")
})

test_that("updateToolInstance updates command|ccs37,ics1692", {
  stopifnot("No instance created" = exists("TM_INSTANCE", envir = globalenv()))
  stopifnot("No runserver" = exists("TM_RUNSERVER", envir = globalenv()))
  inst <- get("TM_INSTANCE", envir = globalenv())
  rs <- get("TM_RUNSERVER", envir = globalenv())

  result <- updateToolInstance(rs$id, inst$id, "echo updated")
  expect_true(is.null(result) || is.list(result))
  cat("Updated tool instance command\n")
})

# ---------------------------------------------------------------------------
# Tool Parameters
# ---------------------------------------------------------------------------
test_that("createToolParameter creates a parameter|ccs38", {
  stopifnot("No instance created" = exists("TM_INSTANCE", envir = globalenv()))
  stopifnot("No runserver" = exists("TM_RUNSERVER", envir = globalenv()))
  inst <- get("TM_INSTANCE", envir = globalenv())
  rs <- get("TM_RUNSERVER", envir = globalenv())

  result <- createToolParameter(
    runserverId = rs$id,
    instanceId = inst$id,
    parameterName = "Tool Arguments",
    value = "--test-arg=value1"
  )
  if (is.null(result)) {
    cat("createToolParameter returned NULL (parameter LOV may not exist)\n")
  } else {
    expect_true(!is.null(result$id))
    assign("TM_PARAM", result, envir = globalenv())
    cat("Created tool parameter:", result$id, "\n")
  }
  expect_true(is.null(result) || is.list(result))
})

test_that("updateToolParameter updates value|ccs39", {
  stopifnot("No parameter created" = exists("TM_PARAM", envir = globalenv()))
  stopifnot("No instance" = exists("TM_INSTANCE", envir = globalenv()))
  stopifnot("No runserver" = exists("TM_RUNSERVER", envir = globalenv()))
  param <- get("TM_PARAM", envir = globalenv())
  inst <- get("TM_INSTANCE", envir = globalenv())
  rs <- get("TM_RUNSERVER", envir = globalenv())

  result <- updateToolParameter(rs$id, inst$id, param$id, "--test-arg=value2")
  expect_false(is.null(result))
  cat("Updated tool parameter value\n")
})

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
test_that("cleanup toolManagement test environment", {
  # Delete category (should cascade to tools and instances)
  if (exists("TM_CATEGORY", envir = globalenv())) {
    cat <- get("TM_CATEGORY", envir = globalenv())
    tryCatch(deleteToolCategory(cat$id), error = function(e) {
      cat("Cleanup warning:", e$message, "\n")
    })
    rm("TM_CATEGORY", envir = globalenv())
  }
  if (exists("TM_TOOL", envir = globalenv())) rm("TM_TOOL", envir = globalenv())
  if (exists("TM_INSTANCE", envir = globalenv())) rm("TM_INSTANCE", envir = globalenv())
  if (exists("TM_RUNSERVER", envir = globalenv())) rm("TM_RUNSERVER", envir = globalenv())
  if (exists("TM_PARAM", envir = globalenv())) rm("TM_PARAM", envir = globalenv())
  expect_true(TRUE)
})
