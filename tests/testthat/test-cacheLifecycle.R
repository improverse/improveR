# Positive tests for the 15 exported load/unload/refresh functions that, until
# now, appeared in exactly one test file - test-errorMessages.R - and only to
# check that they raise when there is no connection (IMR-261).
#
# All 15 are exported. Their contract is a cache lifecycle:
#   loadX(ident)     puts the sub-entity in its cache
#   unloadX(ident)   removes that entry
#   refreshX(ident)  removes it and loads again
#
# The cache is observable through improveR:::searchInCache(), so the contract
# can be asserted directly instead of inferred from a return value. Each family
# keys its cache by resourceId, which is why one helper fits all of them.
#
# Qualified with improveR::: for the internals so the checks run against the
# installed package, not against devtools::load_all() (IMR-255).

CL <- new.env(parent = emptyenv())

cacheEntry <- function(cacheList, resourceId) {
  improveR:::searchInCache(cacheList, resourceId)
}

# One family: its loader, its unloader, its refresher and the cache they share.
# refresh is NULL where the package offers none.
families <- function() list(
  list(id = "auditTrail",         spec = "ics1097", target = "folder",
       load = improveR::loadAuditTrail,         unload = improveR::unloadAuditTrail,
       refresh = improveR::refreshAuditTrail,   cache = improveR:::auditTrailResourceCacheList),
  list(id = "history",            spec = "ics1094", target = "folder",
       load = improveR::loadHistory,            unload = improveR::unloadHistory,
       refresh = improveR::refreshHistory,      cache = improveR:::historyResourceCacheList),
  list(id = "metaData",           spec = "ics1096", target = "folder",
       load = improveR::loadMetaData,           unload = improveR::unloadMetaData,
       refresh = NULL,                          cache = improveR:::metadataResourceCacheList),
  list(id = "references",         spec = "ics1206", target = "folder",
       load = improveR::loadReferences,         unload = improveR::unloadReferences,
       refresh = improveR::refreshReferences,   cache = improveR:::referencesResourceCacheList),
  list(id = "childResources",     spec = "ics1085", target = "folder",
       load = improveR::loadChildResources,     unload = NULL,
       refresh = improveR::refreshChildResources, cache = improveR:::childResourceCacheList),
  list(id = "fullChildResources", spec = "ics1085", target = "folder",
       load = improveR::loadFullChildResources, unload = NULL,
       refresh = improveR::refreshFullChildResources, cache = improveR:::fullChildResourceCacheList),
  list(id = "childSteps",         spec = "ics1205", target = "step",
       load = improveR::loadChildSteps,         unload = improveR::unloadChildSteps,
       refresh = improveR::refreshChildSteps,   cache = improveR:::childStepCacheList),
  list(id = "parentStep",         spec = "ics1209", target = "step",
       load = improveR::loadParentStep,         unload = improveR::unloadParentStep,
       refresh = improveR::refreshParentStep,   cache = improveR:::parentStepCacheList),
  # parentalDescendant was missing from this list. Its load function was the
  # only member of the family the suite ever entered; unload and refresh were
  # among the exported functions never executed (run 25, 2026-09-14). It is a
  # real cache - createCacheList("parentalDescendant"), used by load through
  # genericLoadResourceSubEntities and by unload through removeFromCache - so
  # it belongs here rather than needing a file of its own (IMR-280).
  list(id = "parentalDescendant", spec = "ics2074", target = "folder",
       load = improveR::loadParentalDescendant, unload = improveR::unloadParentalDescendant,
       refresh = improveR::refreshParentalDescendant, cache = improveR:::parentalDescendantCacheList)
)

test_that("setup cacheLifecycle test", {
  improveR::improveConnect()
  improveR::setEditable(TRUE)

  CL$FOLDER <- Sys.getenv("TEST_FOLDER")
  expect_true(nzchar(CL$FOLDER))
  folderRes <- improveR::loadResource(CL$FOLDER)
  expect_false(is.null(folderRes))
  CL$FOLDER_ID <- folderRes$resourceId

  CL$STEP <- Sys.getenv("IMPROVER_STEP")
  if (nzchar(CL$STEP)) {
    stepRes <- tryCatch(improveR::loadResource(CL$STEP), error = function(e) NULL)
    CL$STEP_ID <- if (is.null(stepRes)) NULL else stepRes$resourceId
  } else {
    CL$STEP_ID <- NULL
  }
})

targetOf <- function(fam) {
  if (identical(fam$target, "step")) {
    skip_if(is.null(CL$STEP_ID),
            "IMPROVER_STEP is not set or not loadable - no step to exercise this family on")
    list(ident = CL$STEP, id = CL$STEP_ID)
  } else {
    list(ident = CL$FOLDER, id = CL$FOLDER_ID)
  }
}

for (fam in families()) {
  local({
    f <- fam

    test_that(paste0("load", toupper(substring(f$id,1,1)), substring(f$id,2),
                     " fills its cache|", f$spec), {
      tgt <- targetOf(f)
      loaded <- f$load(tgt$ident)
      skip_if(is.null(loaded),
              paste0("nothing to load for family '", f$id, "' on this resource"))
      expect_false(is.null(cacheEntry(f$cache, tgt$id)),
                   info = paste("cache should hold an entry for", f$id))
    })

    if (!is.null(f$unload)) {
      test_that(paste0("unload", toupper(substring(f$id,1,1)), substring(f$id,2),
                       " removes the cache entry|", f$spec), {
        tgt <- targetOf(f)
        loaded <- f$load(tgt$ident)
        skip_if(is.null(loaded),
                paste0("nothing to load for family '", f$id, "' on this resource"))
        expect_false(is.null(cacheEntry(f$cache, tgt$id)),
                     info = "precondition: the entry is cached before unloading")

        f$unload(tgt$ident)
        expect_null(cacheEntry(f$cache, tgt$id),
                    info = paste("unload should have emptied the cache for", f$id))

        # and the entry comes back on the next load - unload evicts, it does
        # not make the sub-entity unreachable
        again <- f$load(tgt$ident)
        expect_false(is.null(again))
        expect_false(is.null(cacheEntry(f$cache, tgt$id)))
      })
    }

    if (!is.null(f$refresh)) {
      test_that(paste0("refresh", toupper(substring(f$id,1,1)), substring(f$id,2),
                       " returns content and leaves the cache filled|", f$spec), {
        tgt <- targetOf(f)
        first <- f$load(tgt$ident)
        skip_if(is.null(first),
                paste0("nothing to load for family '", f$id, "' on this resource"))

        refreshed <- f$refresh(tgt$ident)
        expect_false(is.null(refreshed),
                     info = paste("refresh should return the reloaded content for", f$id))
        expect_false(is.null(cacheEntry(f$cache, tgt$id)),
                     info = paste("refresh should leave the cache filled for", f$id))
      })
    }
  })
}

test_that("unloadResource evicts the resource itself and it reloads|ics1090", {
  res <- improveR::loadResource(CL$FOLDER)
  expect_false(is.null(res))
  expect_false(is.null(cacheEntry(improveR:::resourceCacheList, res$entityId)))

  improveR::unloadResource(CL$FOLDER)
  expect_null(cacheEntry(improveR:::resourceCacheList, res$entityId))

  again <- improveR::loadResource(CL$FOLDER)
  expect_false(is.null(again))
  expect_equal(again$resourceId, res$resourceId)
})
