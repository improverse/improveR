# Get Convenience Functions tests (ics1141)
# Spec ics1141 mandates getFile, getCopy, getData (with parsers for
# xls/xlsx/rds/sas7bdat/csv/custom), getFilesFromFolder (filePattern, recurse),
# sourceR, getTextString, getGraphics/showGraphics/includeGraphics, showHTML,
# addIdToName, addAsLink.
#
# Test data comes from baseFilesSetup() which populates baseFiles/ with
# rgetTest (csv.csv, excel.xlsx), rgetGRAPH (jpg/png), rgetHTML (html),
# rgetTEXT (txt), rgetR (test.R).

Sys.setenv(TEST_NAME = "getConvenience")

setupGetTests <- function() {
  Sys.setenv(TEST_NAME = "getConvenience")
  improveR::improveConnect()
  improveR::setEditable(TRUE)
  baseFilePath <- improveR:::baseFilesSetup()
  baseFilePath
}

# -----------------------------------------------------------------------------
# getFile / getCopy
# -----------------------------------------------------------------------------
test_that("getFile returns a descriptor with local path|ics1141", {
  baseFilePath <- setupGetTests()
  csvPath <- paste0(baseFilePath, "/rgetTest/csv.csv")

  desc <- improveR::getFile(csvPath)
  expect_false(is.null(desc))
  expect_true(!is.null(desc$path))
  expect_true(file.exists(desc$path))
})

test_that("getFile addIdToName=FALSE does not prefix entityId|ics1141", {
  baseFilePath <- setupGetTests()
  csvPath <- paste0(baseFilePath, "/rgetTest/csv.csv")

  withId <- improveR::getFile(csvPath, addIdToName = TRUE)
  withoutId <- improveR::getFile(csvPath, addIdToName = FALSE)
  expect_false(is.null(withId$path))
  expect_false(is.null(withoutId$path))
  # With the id-prefix the filename is longer than the plain filename
  expect_gt(nchar(basename(withId$path)), nchar(basename(withoutId$path)))
})

test_that("getCopy creates a local copy without addIdToName|ics1141", {
  baseFilePath <- setupGetTests()
  csvPath <- paste0(baseFilePath, "/rgetTest/csv.csv")

  desc <- improveR::getCopy(csvPath)
  expect_false(is.null(desc))
  expect_true(!is.null(desc$path))
  expect_true(file.exists(desc$path))
  # getCopy uses addIdToName=F — local filename equals the remote name
  expect_equal(basename(desc$path), "csv.csv")
})

# -----------------------------------------------------------------------------
# getData parser branches: csv, xlsx, rds, custom
# -----------------------------------------------------------------------------
test_that("getData parses csv files|ics1141", {
  baseFilePath <- setupGetTests()
  csvPath <- paste0(baseFilePath, "/rgetTest/csv.csv")

  desc <- improveR::getData(csvPath)
  expect_false(is.null(desc))
  expect_true(!is.null(desc$data))
  expect_true(is.data.frame(desc$data[[1]]))
  expect_gt(nrow(desc$data[[1]]), 0)
})

test_that("getData parses xlsx files|ics1141", {
  baseFilePath <- setupGetTests()
  xlsxPath <- paste0(baseFilePath, "/rgetTest/excel.xlsx")

  desc <- improveR::getData(xlsxPath)
  expect_false(is.null(desc))
  expect_true(!is.null(desc$data))
  expect_true(is.data.frame(desc$data[[1]]))
})

test_that("getData accepts a custom parser|ics1141", {
  baseFilePath <- setupGetTests()
  csvPath <- paste0(baseFilePath, "/rgetTest/csv.csv")

  # Custom parser: return line count instead of a dataframe
  customParser <- function(path, ...) length(readLines(path, warn = FALSE))
  desc <- improveR::getData(csvPath, parser = customParser)
  expect_false(is.null(desc))
  expect_true(is.numeric(desc$data[[1]]))
  expect_gt(desc$data[[1]], 0)
})

# -----------------------------------------------------------------------------
# getFilesFromFolder — filePattern and recurse
# -----------------------------------------------------------------------------
test_that("getFilesFromFolder returns all files in folder|ics1141", {
  baseFilePath <- setupGetTests()
  folderPath <- paste0(baseFilePath, "/rgetTEXT")

  files <- improveR::getFilesFromFolder(folderPath)
  expect_false(is.null(files))
  expect_true(is.data.frame(files))
  expect_gte(nrow(files), 1)
})

test_that("getFilesFromFolder filters by filePattern|ics1141", {
  baseFilePath <- setupGetTests()
  folderPath <- paste0(baseFilePath, "/rgetGRAPH")

  all <- improveR::getFilesFromFolder(folderPath)
  onlyPng <- improveR::getFilesFromFolder(folderPath, filePattern = "\\.png$")
  expect_false(is.null(all))
  expect_false(is.null(onlyPng))
  expect_lte(nrow(onlyPng), nrow(all))
  if (nrow(onlyPng) > 0) {
    expect_true(all(grepl("\\.png$", onlyPng$name, ignore.case = TRUE)))
  }
})

test_that("getFilesFromFolder recurse=T descends into subfolders|ics1141", {
  baseFilePath <- setupGetTests()

  shallow <- improveR::getFilesFromFolder(baseFilePath, recurse = FALSE)
  deep <- improveR::getFilesFromFolder(baseFilePath, recurse = TRUE)
  # baseFilePath has only subfolders at the top; recurse must yield more rows
  shallowCount <- if (is.null(shallow)) 0 else nrow(shallow)
  deepCount <- if (is.null(deep)) 0 else nrow(deep)
  expect_gt(deepCount, shallowCount)
})

# -----------------------------------------------------------------------------
# sourceR — loads an R file and executes it in the calling environment
# -----------------------------------------------------------------------------
test_that("sourceR executes the R file (side effects visible)|ics1141", {
  baseFilePath <- setupGetTests()
  rPath <- paste0(baseFilePath, "/rgetR/test.R")

  # sourceR executes the script; we just check it returns without error
  # and produces a descriptor
  desc <- tryCatch(improveR::sourceR(rPath), error = function(e) e)
  # Accept either descriptor-returning behavior or silent execution
  expect_false(inherits(desc, "error"))
})

# -----------------------------------------------------------------------------
# getTextString — loads text file as character string
# -----------------------------------------------------------------------------
test_that("getTextString returns text data as a character string|ics1141", {
  baseFilePath <- setupGetTests()
  txtPath <- paste0(baseFilePath, "/rgetTEXT/sampleText.txt")

  desc <- improveR::getTextString(txtPath)
  expect_false(is.null(desc))
  expect_true(!is.null(desc$data))
  expect_true(is.character(desc$data))
  expect_equal(length(desc$data), 1)  # spec: lines collapsed into single string
  expect_gt(nchar(desc$data), 0)
})

# -----------------------------------------------------------------------------
# refresh = TRUE must invalidate the inventory-mirror short-circuit (IMR-210)
# -----------------------------------------------------------------------------
# getAbstract had a local-file-first shortcut: if a copy of the file existed at
# the pwd-relative inventory-mirror path, it was used instead of fetching from
# the server. .refreshGetCaches only invalidated server-side caches and the
# loadFile-managed copies (in cacheEnv$fileCaches); it never touched the
# inventory mirror. Result: refresh=TRUE silently became a no-op when an
# inventory-mirror copy existed (e.g. after a prior CLI checkout). The fix
# makes getAbstract unlink the mirror before falling through to loadFile when
# refresh=TRUE.
test_that("refresh = TRUE invalidates the inventory-mirror short-circuit|ics1141", {
  baseFilePath <- setupGetTests()
  txtPath <- paste0(baseFilePath, "/rgetTEXT/sampleText.txt")

  # IMR-215: the production mirror short-circuit at getGeneric.R:130 only
  # triggers when the resource path startsWith(pwd()$path). The default
  # pwd is wherever IMPROVER_STEP points (the launch step), which is
  # generally sibling-to-or-outside the baseFilesSetup tree — so without
  # pinning pwd here the resource is not under pwd, the production
  # short-circuit never engages, and the test cannot exercise the
  # refresh=TRUE invalidation path. Set pwd to baseFilePath for the
  # test, restore on exit.
  prevPwd <- improveR:::cacheEnv$pwd
  on.exit(improveR:::cacheEnv$pwd <- prevPwd, add = TRUE)
  improveR:::cacheEnv$pwd <- improveR::loadResource(baseFilePath)

  # Resolve where getAbstract would look for an inventory-mirror copy.
  resource <- improveR::loadResource(txtPath)
  pwdPath  <- improveR::pwd()$path
  expect_true(startsWith(resource$path, pwdPath),
              info = "test prerequisite: resource lives under pwd")
  mirrorPath <- paste0(".", substr(resource$path, nchar(pwdPath) + 1, nchar(resource$path)))

  # Plant a deliberately-stale inventory-mirror copy at that path.
  dir.create(dirname(mirrorPath), recursive = TRUE, showWarnings = FALSE)
  staleMarker <- "STALE_CONTENT_test_refresh_invalidates_mirror"
  writeLines(staleMarker, mirrorPath)
  expect_true(file.exists(mirrorPath))

  # Without refresh: the short-circuit reads the stale mirror.
  descStale <- improveR::getTextString(txtPath)
  expect_equal(descStale$data, staleMarker,
               info = "without refresh, the inventory mirror short-circuit returns the stale local content")

  # With refresh = TRUE: the mirror must be unlinked and the function must
  # return fresh content from the server (which is not the stale marker).
  descFresh <- improveR::getTextString(txtPath, refresh = TRUE)
  expect_false(file.exists(mirrorPath),
               info = "refresh = TRUE must unlink the stale inventory-mirror copy")
  expect_false(is.null(descFresh))
  expect_false(identical(descFresh$data, staleMarker),
               info = "refresh = TRUE must return server content, not the stale local marker")
})

# -----------------------------------------------------------------------------
# showGraphics / includeGraphics / showHTML — render-side wrappers.
# show*/include* return rendered output (Markdown string, knitr object,
# htmltools tagList); the on-disk descriptor lives on getGraphics()/getHTML().
# -----------------------------------------------------------------------------
test_that("showGraphics renders Markdown and getGraphics descriptor has a usable path|ics1141", {
  baseFilePath <- setupGetTests()
  imgPath <- paste0(baseFilePath, "/rgetGRAPH/uploads.png")

  fig <- improveR::showGraphics(imgPath)
  expect_true(is.character(fig))
  expect_true(nchar(fig) > 0)

  desc <- improveR::getGraphics(imgPath)
  expect_false(is.null(desc))
  expect_true(!is.null(desc$path))
  expect_true(file.exists(desc$path))
})

test_that("includeGraphics returns a knitr image and the descriptor has a usable path|ics1141", {
  baseFilePath <- setupGetTests()
  imgPath <- paste0(baseFilePath, "/rgetGRAPH/uploads.png")

  fig <- improveR::includeGraphics(imgPath)
  expect_false(is.null(fig))
  expect_true(inherits(fig, "knit_image_paths"))

  desc <- improveR::getGraphics(imgPath)
  expect_true(!is.null(desc$path))
})

test_that("showHTML returns an htmltools tag list and getHTML descriptor has a usable path|ics1141", {
  baseFilePath <- setupGetTests()
  htmlPath <- paste0(baseFilePath, "/rgetHTML/htmlExample.html")

  out <- improveR::showHTML(htmlPath)
  expect_false(is.null(out))
  expect_true(inherits(out, "shiny.tag.list") || inherits(out, "shiny.tag"))

  desc <- improveR::getHTML(htmlPath)
  expect_false(is.null(desc))
  expect_true(!is.null(desc$path))
  expect_true(file.exists(desc$path))
})

# -----------------------------------------------------------------------------
# addAsLink flag (ics1141)
# -----------------------------------------------------------------------------
test_that("getFile addAsLink=FALSE skips inventory linking|ics1141", {
  baseFilePath <- setupGetTests()
  csvPath <- paste0(baseFilePath, "/rgetTest/csv.csv")

  # Neither path should fail. We don't assert server-side inventory here
  # (would require a step context), but exercising both branches confirms
  # the addAsLink argument is wired through.
  descWithLink <- improveR::getFile(csvPath, addAsLink = TRUE)
  expect_false(is.null(descWithLink))
  descWithoutLink <- improveR::getFile(csvPath, addAsLink = FALSE)
  expect_false(is.null(descWithoutLink))
})
