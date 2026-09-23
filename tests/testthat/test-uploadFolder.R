# uploadFolder and createContentCache (ics1210, ics2077, IMR-289)
#
# Two of the thirteen exported functions the suite never entered (C9 section 7).
# Both were probed against the live server on 2026-09-15 before these tests were
# written, because the design's assumptions about the other functions in that
# list had already turned out to be wrong twice (IMR-287, IMR-288). Both work.
#
# The assertions are about what ended up on the server and on disk, not about
# what the call returned. uploadFolder returns nothing at all - it has no return
# value to check - so a test that only called it would assert nothing whatsoever.

Sys.setenv(TEST_NAME = "uploadFolder")

makeLocalTree <- function() {
  root <- file.path(tempdir(), paste0("uf-", uuid::UUIDgenerate()))
  dir.create(file.path(root, "sub"), recursive = TRUE)
  writeLines("top level", file.path(root, "a.txt"))
  writeLines("one level down", file.path(root, "sub", "b.txt"))
  root
}

childrenOf <- function(ident) {
  kids <- improveR::loadChildResources(ident)$data[[1]]
  if (is.null(kids)) data.frame(name = character(), nodeType = character()) else kids
}

setupUploadFolder <- function() {
  Sys.setenv(TEST_NAME = "uploadFolder")
  improveR::improveConnect()
  improveR::setEditable(TRUE)
  basePath <- createFolderPath("uploadFolder")
  target <- improveR::createFolder(
    targetIdent = basePath,
    folderName = paste0("uf-", format(Sys.time(), "%Y%m%d%H%M%S")),
    comment = "uploadFolder test setup"
  )
  stopifnot("createFolder failed" = !is.null(target))
  target
}

test_that("uploadFolder recreates the local tree on the server|ics1210", {
  target <- setupUploadFolder()
  on.exit(tryCatch(improveR::delete(target$resourceId),
                   error = function(e) NULL), add = TRUE)

  local <- makeLocalTree()
  improveR::uploadFolder(target$resourceId, local, comment = "uploadFolder test")

  # The folder is created UNDER the target, named after the local directory.
  top <- childrenOf(target$resourceId)
  expect_equal(nrow(top), 1L)
  expect_equal(as.character(top$name[1]), basename(local))
  expect_equal(as.character(top$nodeType[1]), "Folder")

  # Its contents: the file and the subfolder, both by name. Asserting the count
  # alone would pass on two files, or on two folders.
  inner <- childrenOf(top$resourceId[1])
  expect_setequal(as.character(inner$name), c("a.txt", "sub"))
  expect_equal(as.character(inner$nodeType[inner$name == "a.txt"]), "File")
  expect_equal(as.character(inner$nodeType[inner$name == "sub"]), "Folder")

  # And the recursion went one level further, which is the whole point of the
  # function - a non-recursive upload would pass every assertion above.
  deepest <- childrenOf(inner$resourceId[inner$name == "sub"])
  expect_equal(nrow(deepest), 1L)
  expect_equal(as.character(deepest$name[1]), "b.txt")
})

test_that("uploadFolder warns and creates nothing for a path that is not a folder|ics1210", {
  target <- setupUploadFolder()
  on.exit(tryCatch(improveR::delete(target$resourceId),
                   error = function(e) NULL), add = TRUE)

  notAFolder <- file.path(tempdir(), paste0("uf-file-", uuid::UUIDgenerate(), ".txt"))
  writeLines("this is a file", notAFolder)

  improveR::uploadFolder(target$resourceId, notAFolder, comment = "should not happen")

  expect_equal(nrow(childrenOf(target$resourceId)), 0L,
               info = "a localFolder that is not a directory must create nothing")

  # Same for a path that does not exist at all.
  improveR::uploadFolder(target$resourceId,
                         file.path(tempdir(), paste0("missing-", uuid::UUIDgenerate())),
                         comment = "should not happen either")
  expect_equal(nrow(childrenOf(target$resourceId)), 0L)
})

test_that("createContentCache creates the cache in the given directory|ics2077", {
  improveR::improveConnect()

  cachePath <- file.path(tempdir(), paste0("cc-", uuid::UUIDgenerate()))
  dir.create(cachePath, recursive = TRUE)
  expect_equal(length(list.files(cachePath, all.files = TRUE, no.. = TRUE)), 0L,
               info = "the directory must start empty, or the assertion below proves nothing")

  out <- improveR::createContentCache(cachePath)

  # On disk, not in the return value. The CLI logs a non-zero exit as a warning
  # rather than raising it, so a returned value is no proof of anything.
  expect_true(".improve" %in% list.files(cachePath, all.files = TRUE, no.. = TRUE),
              info = paste0("createContentCache must leave a .improve cache in ", cachePath,
                            "; the directory holds: ",
                            paste(list.files(cachePath, all.files = TRUE, no.. = TRUE),
                                  collapse = ", ")))

  # And the CLI reported the location it was asked for.
  expect_true(any(grepl(cachePath, as.character(out), fixed = TRUE)),
              info = paste0("the CLI output should name the location; it was: ",
                            paste(utils::head(as.character(out), 5), collapse = " | ")))
})
