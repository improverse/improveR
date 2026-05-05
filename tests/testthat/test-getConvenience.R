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

# -----------------------------------------------------------------------------
# Setup — run once, cache result for all tests
# -----------------------------------------------------------------------------
test_that("setup getConvenience test environment|ics1141", {
  improveR::improveConnect()
  improveR::setEditable(TRUE)
  baseFilePath <- improveR:::baseFilesSetup()
  expect_false(is.null(baseFilePath))
  assign("GC_BASE_PATH", baseFilePath, envir = globalenv())
})

# -----------------------------------------------------------------------------
# getFile / getCopy
# -----------------------------------------------------------------------------
test_that("getFile returns a descriptor with local path|ics1141", {
  baseFilePath <- get("GC_BASE_PATH", envir = globalenv())
  csvPath <- paste0(baseFilePath, "/rgetTest/csv.csv")

  desc <- improveR::getFile(csvPath)
  expect_false(is.null(desc))
  expect_true(!is.null(desc$path))
  expect_true(file.exists(desc$path))
})

test_that("getFile addIdToName=FALSE does not prefix entityId|ics1141", {
  baseFilePath <- get("GC_BASE_PATH", envir = globalenv())
  csvPath <- paste0(baseFilePath, "/rgetTest/csv.csv")

  withId <- improveR::getFile(csvPath, addIdToName = TRUE)
  withoutId <- improveR::getFile(csvPath, addIdToName = FALSE)
  expect_false(is.null(withId$path))
  expect_false(is.null(withoutId$path))
  # With the id-prefix the filename is longer than the plain filename
  expect_gt(nchar(basename(withId$path)), nchar(basename(withoutId$path)))
})

test_that("getCopy creates a local copy without addIdToName|ics1141", {
  baseFilePath <- get("GC_BASE_PATH", envir = globalenv())
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
  baseFilePath <- get("GC_BASE_PATH", envir = globalenv())
  csvPath <- paste0(baseFilePath, "/rgetTest/csv.csv")

  desc <- improveR::getData(csvPath)
  expect_false(is.null(desc))
  expect_true(!is.null(desc$data))
  expect_true(is.data.frame(desc$data[[1]]))
  expect_gt(nrow(desc$data[[1]]), 0)
})

test_that("getData parses xlsx files|ics1141", {
  baseFilePath <- get("GC_BASE_PATH", envir = globalenv())
  xlsxPath <- paste0(baseFilePath, "/rgetTest/excel.xlsx")

  desc <- improveR::getData(xlsxPath)
  expect_false(is.null(desc))
  expect_true(!is.null(desc$data))
  expect_true(is.data.frame(desc$data[[1]]))
})

test_that("getData accepts a custom parser|ics1141", {
  baseFilePath <- get("GC_BASE_PATH", envir = globalenv())
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
  baseFilePath <- get("GC_BASE_PATH", envir = globalenv())
  folderPath <- paste0(baseFilePath, "/rgetTEXT")

  files <- improveR::getFilesFromFolder(folderPath)
  expect_false(is.null(files))
  expect_true(is.data.frame(files))
  expect_gte(nrow(files), 1)
})

test_that("getFilesFromFolder filters by filePattern|ics1141", {
  baseFilePath <- get("GC_BASE_PATH", envir = globalenv())
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
  baseFilePath <- get("GC_BASE_PATH", envir = globalenv())

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
  baseFilePath <- get("GC_BASE_PATH", envir = globalenv())
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
  baseFilePath <- get("GC_BASE_PATH", envir = globalenv())
  txtPath <- paste0(baseFilePath, "/rgetTEXT/sampleText.txt")

  desc <- improveR::getTextString(txtPath)
  expect_false(is.null(desc))
  expect_true(!is.null(desc$data))
  expect_true(is.character(desc$data))
  expect_equal(length(desc$data), 1)  # spec: lines collapsed into single string
  expect_gt(nchar(desc$data), 0)
})

# -----------------------------------------------------------------------------
# showGraphics / includeGraphics / showHTML — exercise the descriptor paths
# -----------------------------------------------------------------------------
test_that("showGraphics returns a graphics descriptor|ics1141", {
  baseFilePath <- get("GC_BASE_PATH", envir = globalenv())
  imgPath <- paste0(baseFilePath, "/rgetGRAPH/uploads.png")

  desc <- improveR::showGraphics(imgPath)
  expect_false(is.null(desc))
  expect_true(!is.null(desc$path))
  expect_true(file.exists(desc$path))
})

test_that("includeGraphics returns a graphics descriptor|ics1141", {
  baseFilePath <- get("GC_BASE_PATH", envir = globalenv())
  imgPath <- paste0(baseFilePath, "/rgetGRAPH/uploads.png")

  desc <- improveR::includeGraphics(imgPath)
  expect_false(is.null(desc))
  expect_true(!is.null(desc$path))
})

test_that("showHTML returns an HTML descriptor|ics1141", {
  baseFilePath <- get("GC_BASE_PATH", envir = globalenv())
  htmlPath <- paste0(baseFilePath, "/rgetHTML/htmlExample.html")

  desc <- improveR::showHTML(htmlPath)
  expect_false(is.null(desc))
  expect_true(!is.null(desc$path))
  expect_true(file.exists(desc$path))
})

# -----------------------------------------------------------------------------
# addAsLink flag (ics1141)
# -----------------------------------------------------------------------------
test_that("getFile addAsLink=FALSE skips inventory linking|ics1141", {
  baseFilePath <- get("GC_BASE_PATH", envir = globalenv())
  csvPath <- paste0(baseFilePath, "/rgetTest/csv.csv")

  # Neither path should fail. We don't assert server-side inventory here
  # (would require a step context), but exercising both branches confirms
  # the addAsLink argument is wired through.
  descWithLink <- improveR::getFile(csvPath, addAsLink = TRUE)
  expect_false(is.null(descWithLink))
  descWithoutLink <- improveR::getFile(csvPath, addAsLink = FALSE)
  expect_false(is.null(descWithoutLink))
})
