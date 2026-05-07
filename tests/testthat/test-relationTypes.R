# Relation Type Admin CRUD tests (ics1694, ics1699, ics1700)
# Tests create, update, and delete of relation types via the
# configuration/relationTypeLov REST endpoint.

Sys.setenv(TEST_NAME = "relationTypes")

test_that("setup relation types test", {
  improveR::improveConnect()
  improveR::setEditable(TRUE)
  expect_true(improveR::improveConnected())
})

test_that("createRelationType creates a new type visible in loadRelationTypes|ics1694,ics1697", {
  typeName <- paste0("testType-", format(Sys.time(), "%H%M%S"))
  reverseName <- paste0("reverse-", typeName)

  created <- improveR::createRelationType(
    name = typeName,
    reverseName = reverseName,
    description = "relation type created by automated test"
  )
  expect_false(is.null(created))
  expect_false(is.null(created$id))
  expect_equal(created$name, typeName)
  expect_equal(created$reverseName, reverseName)
  cat("Created relation type:", created$id, "(", typeName, ")\n")

  # Verify it appears in refreshed list
  allTypes <- improveR::loadRelationTypes()
  expect_false(is.null(allTypes))
  expect_true(created$id %in% allTypes$id)

  assign("RT_CREATED_ID", created$id, envir = globalenv())
  assign("RT_CREATED_NAME", typeName, envir = globalenv())
})

test_that("createRelationType is idempotent (same name returns existing)|ics1694", {
  stopifnot("No type created" = exists("RT_CREATED_NAME", envir = globalenv()))
  typeName <- get("RT_CREATED_NAME", envir = globalenv())
  originalId <- get("RT_CREATED_ID", envir = globalenv())

  second <- improveR::createRelationType(
    name = typeName,
    reverseName = paste0("reverse-", typeName),
    description = "duplicate creation attempt"
  )
  expect_false(is.null(second))
  expect_equal(second$id, originalId)
})

# updateRelationType — server does not support PUT on configuration/relationTypeLov/{id} yet.
# Uncomment when the endpoint is available.
# test_that("updateRelationType changes name and description|ics1699,ics1697", {
#   stopifnot("No type created" = exists("RT_CREATED_ID", envir = globalenv()))
#   rtId <- get("RT_CREATED_ID", envir = globalenv())
#
#   newName <- paste0("updated-", format(Sys.time(), "%H%M%S"))
#   newReverse <- paste0("reverse-", newName)
#
#   updated <- improveR::updateRelationType(
#     relationTypeId = rtId,
#     name = newName,
#     reverseName = newReverse,
#     description = "updated by automated test"
#   )
#   stopifnot("updateRelationType returned NULL (endpoint may not support PUT)" = !is.null(updated))
#
#   # Verify the change via loadRelationTypes
#   allTypes <- improveR::refreshRelationTypes()
#   expect_false(is.null(allTypes))
#   row <- allTypes[allTypes$id == rtId, ]
#   expect_equal(nrow(row), 1)
#   expect_equal(row$name, newName)
#   cat("Updated relation type:", rtId, "->", newName, "\n")
# })

test_that("deleteRelationType removes type from loadRelationTypes|ics1700,ics1697", {
  stopifnot("No type created" = exists("RT_CREATED_ID", envir = globalenv()))
  rtId <- get("RT_CREATED_ID", envir = globalenv())

  result <- improveR::deleteRelationType(rtId)
  expect_true(result)

  # Verify it's gone
  allTypes <- improveR::refreshRelationTypes()
  if (!is.null(allTypes)) {
    expect_false(rtId %in% allTypes$id)
  }
  cat("Deleted relation type:", rtId, "\n")
})

test_that("cleanup relation types test", {
  if (exists("RT_CREATED_ID", envir = globalenv())) {
    # Safety net: delete if previous test didn't clean up
    tryCatch(improveR::deleteRelationType(get("RT_CREATED_ID", envir = globalenv())),
             error = function(e) NULL)
    rm("RT_CREATED_ID", envir = globalenv())
  }
  if (exists("RT_CREATED_NAME", envir = globalenv())) rm("RT_CREATED_NAME", envir = globalenv())
  expect_true(TRUE)
})
