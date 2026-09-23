# Test Favorites Functions
# Tests: loadFavorites, loadFavoriteChildren, addFavoriteLink,
#        createFavoriteFolder, removeFavorite

ensureTestFolder <- function() {
  if (!exists("TEST_FOLDER", envir = globalenv())) {
    improveR::improveConnect()
    improveR::setEditable(TRUE)
    basePath <- createFolderPath("favorites")
    testFolder <- improveR::createFolder(
      targetIdent = basePath,
      folderName = paste0("test-favorites-", uniqueTag()),
      comment = "favorites test setup"
    )
    assign("TEST_FOLDER", testFolder, envir = globalenv())
  }
}

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------
test_that("setup favorites test environment", {
  improveR::improveConnect()
  improveR::setEditable(TRUE)

  basePath <- createFolderPath("favorites")
  testFolder <- improveR::createFolder(
    targetIdent = basePath,
    folderName = paste0("test-favorites-", uniqueTag()),
    comment = "favorites test setup"
  )
  expect_false(is.null(testFolder))
  assign("TEST_FOLDER", testFolder, envir = globalenv())

  testFile <- improveR::createFile(
    targetIdent = testFolder$resourceId,
    fileName = "fav-test-file.txt",
    comment = "test file for favorites"
  )
  expect_false(is.null(testFile))
  assign("TEST_FILE", testFile, envir = globalenv())
})

# ---------------------------------------------------------------------------
# loadFavorites | ics1799
# ---------------------------------------------------------------------------
test_that("loadFavorites returns data frame or NULL|ics1799", {
  ensureTestFolder()
  favorites <- improveR::loadFavorites()
  if (!is.null(favorites)) {
    expect_true(is.data.frame(favorites))
  }
})

# ---------------------------------------------------------------------------
# createFavoriteFolder | ics1805
# ---------------------------------------------------------------------------
test_that("createFavoriteFolder creates a folder in favorites|ics1805", {
  ensureTestFolder()
  folderName <- paste0("TestFavFolder-", uniqueTag(6))
  result <- improveR::createFavoriteFolder(name = folderName)
  requireServerCall(result, "createFavoriteFolder")
  expect_false(is.null(result))
  assign("FAV_FOLDER", result, envir = globalenv())
  cat("Created favorites folder:", folderName, "\n")
})

# ---------------------------------------------------------------------------
# addFavoriteLink | ics1803
# ---------------------------------------------------------------------------
test_that("addFavoriteLink adds a link to favorites|ics1803", {
  ensureTestFolder()
  testFile <- get("TEST_FILE", envir = globalenv())

  # Unique per run. This was the one fixed name in a file whose every other
  # fixture timestamps itself, and it is the only thing here that could collide
  # with leftovers from an earlier run. Run 29 died before its teardown, and
  # runs 30 and 31 then both failed this block with
  #
  #   HTTP 500: A resource with name fav-link-test already exists.
  #
  # A fixture that cannot survive its predecessor's debris makes every run
  # depend on the one before it - the same dependency IMR-275 removed from the
  # preventive cleanup. REQ-REPLAY-001 FR-RPL-025 states it as a requirement
  # for the customer package: cases SHALL be collision-free, with generated
  # names that cannot clash with existing content or a concurrent run.
  linkName <- paste0("fav-link-test-", format(Sys.time(), "%Y%m%d%H%M%S"))
  result <- improveR::addFavoriteLink(
    targetId = testFile$resourceId,
    name = linkName
  )
  requireServerCall(result, "addFavoriteLink")
  expect_false(is.null(result))
  assign("FAV_LINK", result, envir = globalenv())
  assign("FAV_LINK_NAME", linkName, envir = globalenv())
  cat("Added favorite link:", linkName, "\n")
})

# ---------------------------------------------------------------------------
# loadFavoriteChildren | ics1801
# ---------------------------------------------------------------------------
test_that("loadFavoriteChildren returns children|ics1801", {
  ensureTestFolder()
  children <- improveR::loadFavoriteChildren()
  if (!is.null(children)) {
    expect_true(is.data.frame(children))
    cat("Favorite children found:", nrow(children), "\n")
  }
})

# ---------------------------------------------------------------------------
# removeFavorite | ics1800
# ---------------------------------------------------------------------------
test_that("removeFavorite removes a favorite|ics1800", {
  ensureTestFolder()
  if (!exists("FAV_LINK", envir = globalenv())) skip("No favorite link created")
  favLink <- get("FAV_LINK", envir = globalenv())

  resourceId <- favLink$resourceId
  if (is.null(resourceId)) skip("No resourceId in favorite link response")

  result <- improveR::removeFavorite(resourceId)
  expect_true(result)
  cat("Removed favorite:", resourceId, "\n")
})

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
test_that("cleanup favorites test environment", {
  if (exists("FAV_FOLDER", envir = globalenv())) {
    favFolder <- get("FAV_FOLDER", envir = globalenv())
    tryCatch({
      if (!is.null(favFolder$resourceId)) {
        improveR::removeFavorite(favFolder$resourceId)
      }
    }, error = function(e) {
      cat("Cleanup warning:", e$message, "\n")
    })
    rm("FAV_FOLDER", envir = globalenv())
  }
  if (exists("TEST_FOLDER", envir = globalenv())) {
    testFolder <- get("TEST_FOLDER", envir = globalenv())
    tryCatch({
      improveR::delete(testFolder$resourceId, comment = "favorites test cleanup")
    }, error = function(e) {
      cat("Cleanup warning:", e$message, "\n")
    })
    rm("TEST_FOLDER", envir = globalenv())
  }
  if (exists("TEST_FILE", envir = globalenv())) rm("TEST_FILE", envir = globalenv())
  if (exists("FAV_LINK", envir = globalenv())) rm("FAV_LINK", envir = globalenv())
  expect_true(TRUE)
})
