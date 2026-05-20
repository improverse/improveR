# IMR-216: authenticatedREST returns NULL on every non-2xx (contract preserved),
# and lastRestError() exposes status_code, url, method, and a body snippet so
# callers can inspect WHY a NULL came back. These tests exercise the helpers
# directly (no live server needed).

Sys.setenv(TEST_NAME = "lastRestError")

test_that("setLastRestError + lastRestError round-trip carries status/url/method/message|ics1082,imr216", {
  improveR:::clearLastRestError()
  improveR:::setLastRestError(503, "http://repo/api/x", "GET",
                              "GET http://repo/api/x -> HTTP 503: Service unavailable")
  err <- improveR::lastRestError()
  expect_false(is.null(err))
  expect_equal(err$status_code, 503)
  expect_equal(err$url, "http://repo/api/x")
  expect_equal(err$method, "GET")
  expect_true(grepl("Service unavailable", err$message, fixed = TRUE))
  expect_true(inherits(err$timestamp, "POSIXt"))
})

test_that("clearLastRestError clears state|ics1082,imr216", {
  improveR:::setLastRestError(500, "u", "GET", "boom")
  expect_false(is.null(improveR::lastRestError()))
  improveR:::clearLastRestError()
  expect_null(improveR::lastRestError())
})

test_that("lastRestError starts as NULL on fresh package state|ics1082,imr216", {
  improveR:::clearLastRestError()
  expect_null(improveR::lastRestError())
})

test_that("multiple non-2xx calls overwrite — most recent wins|ics1082,imr216", {
  improveR:::setLastRestError(500, "u1", "GET", "first")
  improveR:::setLastRestError(404, "u2", "POST", "second")
  err <- improveR::lastRestError()
  expect_equal(err$status_code, 404)
  expect_equal(err$url, "u2")
  expect_equal(err$method, "POST")
  expect_true(grepl("second", err$message, fixed = TRUE))
})

test_that("NA status_code is recordable for pre-call validation errors (undefined restType)|ics1082,imr216", {
  improveR:::setLastRestError(NA, "/x", "WHATEVER", "undefined REST method 'WHATEVER'")
  err <- improveR::lastRestError()
  expect_true(is.na(err$status_code))
  expect_equal(err$url, "/x")
  expect_true(grepl("undefined", err$message, fixed = TRUE))
})
