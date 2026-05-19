# Unit tests for .processVariablesContentToDf — the helper that converts
# the parsed JSON body of GET/POST /processes/{pid}/variables into a
# one-row-per-variable data frame.
#
# IMR-213: the previous implementation used base::rbind on rows of
# as.data.frame(row), which crashes with
# "numbers of columns of arguments do not match" when the server returns
# variables in mid-creation state where some have optional fields
# populated (e.g. valueResourceId) and others don't. These tests pin the
# rbind.fill semantic so heterogeneous field sets are accepted.

Sys.setenv(TEST_NAME = "processVariablesContentToDf")

test_that(".processVariablesContentToDf tolerates heterogeneous field sets across variables|ics2049,imr213", {
  # Real shape captured 2026-05-19 from 5310 backend during
  # createTemplate()$realise() repro of the rbind crash.
  content <- list(
    list(
      id           = "523100CC11D5E6E7E063020011AC6761",
      position     = 1L,
      name         = "command-file",
      variableType = "fileRef",
      processId    = "3E28242DAAA04C258F82E4908D02FE95"
      # NB: no valueResourceId — variable not yet bound to a value
    ),
    list(
      id              = "523100CC11D6E6E7E063020011AC6761",
      position        = 1L,
      name            = "dataset",
      variableType    = "fileRef",
      valueResourceId = "D9577E80097F4E7F9E11271B2C08D9AE",
      processId       = "3E28242DAAA04C258F82E4908D02FE95"
    )
  )
  df <- improveR:::.processVariablesContentToDf(content)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)
  expect_true("valueResourceId" %in% names(df))
  expect_equal(df$name, c("command-file", "dataset"))
  # Missing optional field is NA on the row that didn't carry it
  expect_true(is.na(df$valueResourceId[df$name == "command-file"]))
  expect_equal(df$valueResourceId[df$name == "dataset"],
               "D9577E80097F4E7F9E11271B2C08D9AE")
})

test_that(".processVariablesContentToDf handles a homogeneous list-of-objects|ics2049,imr213", {
  content <- list(
    list(id = "A", position = 1L, name = "v1", variableType = "fileRef",
         valueResourceId = "X", processId = "P"),
    list(id = "B", position = 2L, name = "v2", variableType = "fileRef",
         valueResourceId = "Y", processId = "P")
  )
  df <- improveR:::.processVariablesContentToDf(content)
  expect_equal(nrow(df), 2L)
  expect_equal(df$name, c("v1", "v2"))
  expect_equal(df$valueResourceId, c("X", "Y"))
})

test_that(".processVariablesContentToDf still wraps a flat single object as one row|ics2049,imr213", {
  # Preserves the IMR-209 fix: a POST response is a flat named list, not
  # a list-of-objects. The detector at step.R:312-316 must still wrap it
  # before flattening so we don't get five rows of one column.
  content <- list(
    id = "C", position = 1L, name = "v3",
    variableType = "fileRef", valueResourceId = "Z", processId = "P"
  )
  df <- improveR:::.processVariablesContentToDf(content)
  expect_equal(nrow(df), 1L)
  expect_equal(df$name, "v3")
  expect_equal(df$valueResourceId, "Z")
})

test_that(".processVariablesContentToDf returns empty df for length-0 input|ics2049,imr213", {
  df <- improveR:::.processVariablesContentToDf(list())
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
})

test_that(".processVariablesContentToDf returns NULL for NULL input (preserves getProcessFileVariables contract)|ics2049,imr213", {
  expect_null(improveR:::.processVariablesContentToDf(NULL))
})
