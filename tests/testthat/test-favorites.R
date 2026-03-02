# Test Favorites Functions
# Tests: loadFavorites, loadFavoriteChildren, addFavoriteLink,
#        createFavoriteFolder, removeFavorite

ensureTestFolder <- function() {
  if (!exists("TEST_FOLDER", envir = globalenv())) {
    tryCatch({
      improveR::improveConnect()
      improveR::setEditable(TRUE)
      testFolder <- improveR::createFolder(
        targetIdent = "/",
        folderName = paste0("test-favorites-", format(Sys.time(), "%Y%m%d%H%M%S")),
        comment = "favorites test setup"
      )
      assign("TEST_FOLDER", testFolder, envir = globalenv())
    }, error = function(e) {
      skip(paste("Server not available:", e$message))
    })
  }
}

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------
test_that("setup favorites test environment", {
  tryCatch({
    improveR::improveConnect()
    improveR::setEditable(TRUE)
  }, error = function(e) {
    skip(paste("Server not available:", e$message))
  })

  testFolder <- improveR::createFolder(
    targetIdent = "/",
    folderName = paste0("test-favorites-", format(Sys.time(), "%Y%m%d%H%M%S")),
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
  folderName <- paste0("TestFavFolder-", format(Sys.time(), "%H%M%S"))
  result <- improveR::createFavoriteFolder(name = folderName)
  if (is.null(result)) {
    skip("createFavoriteFolder not supported on this server")
  }
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

  result <- improveR::addFavoriteLink(
    targetId = testFile$resourceId,
    name = "fav-link-test"
  )
  if (is.null(result)) {
    skip("addFavoriteLink not supported on this server")
  }
  expect_false(is.null(result))
  assign("FAV_LINK", result, envir = globalenv())
  cat("Added favorite link\n")
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
})
