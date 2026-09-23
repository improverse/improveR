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
  typeName <- paste0("testType-", uniqueTag(6))
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

# updateRelationType: the server has no PUT on this endpoint, and this block
# says so as an assertion rather than as a comment (IMR-289).
#
# It used to be a commented-out test with the note "uncomment when the endpoint
# is available". A commented-out test asserts nothing and ages silently -
# nobody would have noticed the day the endpoint appeared. Measured on
# repository 4.4.5-4 on 2026-09-15: still 405.
#
# Written this way the limitation is in the run record instead of in a comment,
# and the test FAILS the moment the server gains the endpoint - which is exactly
# when somebody should look at it.

test_that("updateRelationType is not supported by this server, and says so|ics1699", {
  stopifnot("No type created" = exists("RT_CREATED_ID", envir = globalenv()))
  rtId <- get("RT_CREATED_ID", envir = globalenv())

  newName <- paste0("updated-", format(Sys.time(), "%H%M%S"))
  updated <- improveR::updateRelationType(
    relationTypeId = rtId,
    name = newName,
    reverseName = paste0("reverse-", newName),
    description = "updated by automated test"
  )

  expect_null(updated,
              info = paste0("If this is no longer NULL the server has gained PUT on ",
                            "configuration/relationTypeLov/{id}. Replace this block with ",
                            "the positive test: update, refresh, assert name and ",
                            "reverseName by value (IMR-289)."))

  err <- improveR::lastRestError()
  expect_false(is.null(err))
  expect_equal(err$status_code, 405L)
  expect_equal(err$method, "PUT")
  expect_true(grepl("relationTypeLov", err$url, fixed = TRUE))

  # And the type is unchanged - a rejected write must not have altered anything.
  allTypes <- improveR::refreshRelationTypes()
  expect_false(is.null(allTypes))
  row <- allTypes[allTypes$id == rtId, ]
  expect_equal(nrow(row), 1L)
  expect_false(identical(as.character(row$name), newName))
})

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
