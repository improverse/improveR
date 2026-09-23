# The deprecated update* aliases: 27 exported entry points that nothing executed
#
# These were counted as intentionally uncovered, on the reasoning that "testing
# the alias would test .Deprecated(), not behaviour".
#
# IMR-288 is the reason that reasoning does not hold. getStepTemplate was an
# exported, specified, documented function that sourced a file the commit making
# it public had deleted. It could not work for any input, for fourteen months,
# and nothing noticed because nothing ever executed it.
#
# Each alias here is exactly that shape:
#
#   updateAuditTrail <- function(...) {
#     .Deprecated("refreshAuditTrail")
#     refreshAuditTrail(...)
#   }
#
# An alias whose target is renamed or removed is IMR-288 again, 27 times over.
# What is asserted is not that .Deprecated() works - it is that every one of
# these 27 public entry points still reaches a function that exists, and passes
# its arguments there unchanged.
#
# No server. The target is replaced by a recorder, so nothing reaches the
# network and nothing depends on a repository.
#
# 18 of the 27 are exported and 9 are internal - and the 9 are internal
# CONSISTENTLY, target as well as alias, which is why they are not part of the
# 255 exported functions the coverage figure counts. The first run of this file
# asserted all 27 were exported and was wrong about nine of them; what is
# asserted now is the invariant that actually matters, that an alias is exactly
# as public as the function it delegates to. An exported target behind an
# internal alias would be a public entry point quietly withdrawn; an exported
# alias in front of an internal target would export a function by the back door.

# alias -> the function it must delegate to. Adding an alias is one row.
ALIASES <- rbind(
  c("updateAuditTrail", "refreshAuditTrail"),
  c("updateChildResources", "refreshChildResources"),
  c("updateChildSteps", "refreshChildSteps"),
  c("updateFile", "refreshFile"),
  c("updateFullChildResources", "refreshFullChildResources"),
  c("updateGridArguments", "refreshGridArguments"),
  c("updateHistory", "refreshHistory"),
  c("updateParentalDescendant", "refreshParentalDescendant"),
  c("updateMetaDataDefinitions", "refreshMetaDataDefinitions"),
  c("updateMetaData", "refreshMetaData"),
  c("updateProcessesForStep", "refreshProcessesForStep"),
  c("updateProcessGridArguments", "refreshProcessGridArguments"),
  c("updateProcessVariables", "refreshProcessVariables"),
  c("updateProcessRuns", "refreshProcessRuns"),
  c("updateReviews", "refreshReviews"),
  c("updateReviewers", "refreshReviewers"),
  c("updateReviewEntries", "refreshReviewEntries"),
  c("updateReviewComments", "refreshReviewComments"),
  c("updateReferences", "refreshReferences"),
  c("updateResource", "refreshResource"),
  c("updateRelationTypes", "refreshRelationTypes"),
  c("updateResourceRelations", "refreshResourceRelations"),
  c("updateParentStep", "refreshParentStep"),
  c("updateRunservers", "refreshRunservers"),
  c("updateToolsForRunserver", "refreshToolsForRunserver"),
  c("updateToolCategories", "refreshToolCategories"),
  c("updateToolsForCategory", "refreshToolsForCategory")
)
colnames(ALIASES) <- c("alias", "target")

test_that("every deprecated alias is exactly as public as its target|ics2276,IMR-301", {
  ns       <- asNamespace("improveR")
  exported <- getNamespaceExports("improveR")

  for (i in seq_len(nrow(ALIASES))) {
    alias  <- ALIASES[i, "alias"]
    target <- ALIASES[i, "target"]

    expect_true(exists(alias, envir = ns, inherits = FALSE),
                info = sprintf("%s does not exist", alias))
    expect_true(exists(target, envir = ns, inherits = FALSE),
                info = sprintf("%s delegates to %s, which does not exist", alias, target))
    expect_true(is.function(get0(target, envir = ns)),
                info = sprintf("%s delegates to %s, which is not a function", alias, target))
    expect_identical(alias %in% exported, target %in% exported,
                     info = sprintf("%s and %s do not agree on being exported", alias, target))
  }
})

test_that("the 18 public aliases are still public|ics2276,IMR-301", {
  # Named individually and not derived, so that an alias silently dropped from
  # NAMESPACE fails here instead of quietly agreeing with its target.
  public <- c(
    "updateAuditTrail", "updateChildResources", "updateChildSteps", "updateFile",
    "updateFullChildResources", "updateHistory", "updateParentalDescendant",
    "updateMetaDataDefinitions", "updateMetaData", "updateReviews", "updateReviewers",
    "updateReviewEntries", "updateReviewComments", "updateReferences", "updateResource",
    "updateRelationTypes", "updateResourceRelations", "updateParentStep"
  )
  expect_length(public, 18L)
  expect_true(all(public %in% ALIASES[, "alias"]))
  expect_true(all(public %in% getNamespaceExports("improveR")))
})

test_that("every deprecated alias forwards its arguments to its target|ics2276,IMR-301", {
  ns <- asNamespace("improveR")

  for (i in seq_len(nrow(ALIASES))) {
    alias  <- ALIASES[i, "alias"]
    target <- ALIASES[i, "target"]
    fn     <- get(alias, envir = ns)

    received <- NULL
    recorder <- function(...) {
      received <<- list(...)
      "TARGET-REACHED"
    }

    # The call is built rather than written so that the target name can vary.
    # as.call puts `fn("sentinel-ident", ...)` in as the EXPRESSION of
    # with_mocked_bindings' code argument, so it is evaluated inside the mock -
    # passing quote(...) as a value would hand back the unevaluated call.
    cl <- as.call(c(
      quote(testthat::with_mocked_bindings),
      as.call(list(fn, "sentinel-ident", nested = TRUE)),
      stats::setNames(list(recorder), target),
      .package = "improveR"
    ))

    result <- suppressWarnings(eval(cl))

    expect_identical(result, "TARGET-REACHED",
                     info = sprintf("%s did not reach %s", alias, target))
    expect_identical(received[[1]], "sentinel-ident",
                     info = sprintf("%s did not forward its first argument", alias))
    expect_identical(received$nested, TRUE,
                     info = sprintf("%s did not forward its named arguments", alias))
  }
})

test_that("every deprecated alias says it is deprecated|ics2276,IMR-301", {
  ns <- asNamespace("improveR")

  for (i in seq_len(nrow(ALIASES))) {
    alias  <- ALIASES[i, "alias"]
    target <- ALIASES[i, "target"]
    fn     <- get(alias, envir = ns)

    cl <- as.call(c(
      quote(testthat::with_mocked_bindings),
      as.call(list(fn, "sentinel-ident")),
      stats::setNames(list(function(...) NULL), target),
      .package = "improveR"
    ))

    warned <- character(0)
    withCallingHandlers(
      eval(cl),
      warning = function(w) {
        warned <<- c(warned, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_true(any(grepl(target, warned, fixed = TRUE)),
                info = sprintf("%s did not name %s in its deprecation warning", alias, target))
  }
})

test_that("the alias table covers every .Deprecated alias in the package|IMR-301", {
  # Guards the table itself: a new alias added to the package without a row here
  # would otherwise be uncovered again, silently.
  ns    <- asNamespace("improveR")
  found <- character(0)

  # ls() on the namespace, not getNamespaceExports(): nine of the aliases are
  # internal, and looking only at exports would miss exactly those.
  for (nm in ls(envir = ns, all.names = TRUE)) {
    obj <- get0(nm, envir = ns)
    if (!is.function(obj)) next
    src <- paste(deparse(body(obj)), collapse = " ")
    if (grepl(".Deprecated(", src, fixed = TRUE)) found <- c(found, nm)
  }

  expect_setequal(sort(found), sort(ALIASES[, "alias"]))
})
