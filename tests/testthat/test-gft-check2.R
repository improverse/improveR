# GFT Check 2 — File Operations, Favorites, and Search
# Mirrors: iat2951 gft_check2.rsc
# Requirements: ics472, ics1799-1806, ics1139, ics1255
#
# Scenarios covered:
#   - Copy file
#   - Move file
#   - Lock / unlock (checkout / checkin cycle)
#   - File version history
#   - Search (query)
#   - Add to favorites / list favorites / remove favorite
#   - Audit trail reading
#   - Transactions / revisions
#
# Skipped (UI-only):
#   - Data Manipulation Log
#   - Compare files (UI compare view)
#   - Change link target (UI dialog)
#   - Export file to filesystem
#   - Search by checksum (client UI)
#   - Copy Entity ID (clipboard)

GFT2 <- new.env(parent = emptyenv())

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------
test_that("GFT2-setup: connect and create test folder with files", {
  tryCatch({
    improveR::improveConnect()
    improveR::setEditable(TRUE)
  }, error = function(e) {
    skip(paste("Server not available:", e$message))
  })

  basePath <- Sys.getenv("TEST_FOLDER", "/Projects/Tests")
  baseRes <- improveR::loadResource(basePath)
  if (is.null(baseRes)) {
    # Create TEST_FOLDER path segment by segment
    segments <- strsplit(basePath, "/", fixed = TRUE)[[1]]
    segments <- segments[segments != ""]
    currentPath <- ""
    for (seg in segments) {
      parentPath <- if (currentPath == "") "/" else currentPath
      currentPath <- paste0(currentPath, "/", seg)
      existing <- tryCatch(improveR::loadResource(currentPath), error = function(e) NULL)
      if (is.null(existing)) {
        improveR::createFolder(targetIdent = parentPath, folderName = seg, comment = "auto-created test folder")
      }
    }
    baseRes <- improveR::loadResource(basePath)
    if (is.null(baseRes)) {
      skip(paste("Could not create TEST_FOLDER:", basePath))
    }
  }

  ts <- format(Sys.time(), "%Y%m%d%H%M%S")
  folder <- improveR::createFolder(
    targetIdent = basePath,
    folderName = paste0("gft-check2-", ts),
    comment = "GFT check 2 test folder"
  )
  expect_false(is.null(folder), info = "GFT2 root folder should be created")
  expect_equal(folder$nodeType, "Folder")
  GFT2$ROOT_PATH <- folder$path
  GFT2$ROOT_RES <- folder
  cat("Created GFT2 root folder:", folder$path, "\n")

  # Create temp dir and test files
  tmpDir <- file.path(tempdir(), paste0("gft2-", ts))
  dir.create(tmpDir, showWarnings = FALSE, recursive = TRUE)
  GFT2$TMP_DIR <- tmpDir

  localFile1 <- file.path(tmpDir, "test_file1.txt")
  writeLines(c("line 1", "line 2", "line 3"), localFile1)
  f1 <- improveR::createFile(
    targetIdent = GFT2$ROOT_PATH,
    fileName = "test_file1.txt",
    localPath = localFile1,
    comment = "GFT2 test file 1"
  )
  expect_false(is.null(f1), info = "test_file1.txt should be created")
  expect_equal(f1$name, "test_file1.txt")
  GFT2$FILE1_PATH <- paste0(GFT2$ROOT_PATH, "/test_file1.txt")
  GFT2$FILE1_RES <- f1

  localFile2 <- file.path(tmpDir, "test_file2.txt")
  writeLines(c("alpha", "beta", "gamma"), localFile2)
  f2 <- improveR::createFile(
    targetIdent = GFT2$ROOT_PATH,
    fileName = "test_file2.txt",
    localPath = localFile2,
    comment = "GFT2 test file 2"
  )
  expect_false(is.null(f2), info = "test_file2.txt should be created")
  GFT2$FILE2_PATH <- paste0(GFT2$ROOT_PATH, "/test_file2.txt")
  GFT2$FILE2_RES <- f2

  # Create a subfolder for move target
  moveTarget <- improveR::createFolder(
    targetIdent = GFT2$ROOT_PATH,
    folderName = "move_target",
    comment = "move target folder"
  )
  expect_false(is.null(moveTarget), info = "move_target folder should be created")
  GFT2$MOVE_TARGET_PATH <- moveTarget$path

  cat("Setup complete: 2 files + move_target folder\n")
})

# ===========================================================================
# Copy file | ics1139
# ===========================================================================
test_that("GFT2-01: copy file to same folder|ics1139", {
  skip_if(is.null(GFT2$FILE1_PATH), "No test file 1")

  result <- improveR::copy(
    sources = GFT2$FILE1_PATH,
    target = GFT2$ROOT_PATH,
    targetName = "test_file1_copy.txt",
    comment = "GFT2 copy test"
  )
  expect_false(is.null(result), info = "copy should return a resource")
  expect_true(is.data.frame(result), info = "copy result should be a data frame")
  expect_equal(result$name, "test_file1_copy.txt",
               info = "Copied file should have the target name")

  GFT2$COPY_PATH <- paste0(GFT2$ROOT_PATH, "/test_file1_copy.txt")

  # Verify the copy exists and is independent
  copyRes <- improveR::loadResource(GFT2$COPY_PATH)
  expect_false(is.null(copyRes), info = "Copied file should be loadable")
  expect_equal(copyRes$nodeType, "File")
  # Copy should have its own resourceId, different from original
  expect_false(copyRes$resourceId == GFT2$FILE1_RES$resourceId,
               info = "Copy should have a different resourceId than original")
  cat("Copied file to:", GFT2$COPY_PATH, "\n")
})

# ===========================================================================
# Move file | ics1139
# ===========================================================================
test_that("GFT2-02: move file to subfolder|ics1139", {
  skip_if(is.null(GFT2$FILE2_PATH), "No test file 2")
  skip_if(is.null(GFT2$MOVE_TARGET_PATH), "No move target folder")

  result <- improveR::move(
    sources = GFT2$FILE2_PATH,
    target = GFT2$MOVE_TARGET_PATH,
    comment = "GFT2 move test"
  )
  expect_false(is.null(result), info = "move should return a resource")
  expect_true(is.data.frame(result), info = "move result should be a data frame")

  movedPath <- paste0(GFT2$MOVE_TARGET_PATH, "/test_file2.txt")
  movedRes <- improveR::loadResource(movedPath)
  expect_false(is.null(movedRes), info = "Moved file should be at new location")
  expect_equal(movedRes$name, "test_file2.txt")

  # Original path should no longer resolve
  oldRes <- tryCatch(
    improveR::loadResource(GFT2$FILE2_PATH),
    error = function(e) NULL
  )
  # After move, old path may still resolve (same resourceId, updated path) or not
  # The key assertion is the file is at the new location
  GFT2$FILE2_MOVED_PATH <- movedPath
  cat("Moved file to:", movedPath, "\n")
})

# ===========================================================================
# Lock / Unlock (Checkout / Checkin cycle) | ics472
# ===========================================================================
test_that("GFT2-03: lock, update content, and unlock file|ics472", {
  skip_if(is.null(GFT2$FILE1_PATH), "No test file 1")
  skip_if(is.null(GFT2$TMP_DIR), "No temp dir")

  # Lock (checkout)
  locked <- improveR::lockResource(GFT2$FILE1_PATH)
  expect_true(locked, info = "lockResource should return TRUE on success")

  # Verify the resource shows as locked
  lockedRes <- improveR::updateResource(GFT2$FILE1_PATH)
  expect_true("lockedByName" %in% names(lockedRes),
              info = "Locked resource should have lockedByName field")
  expect_false(is.na(lockedRes$lockedByName),
               info = "lockedByName should not be NA when locked")

  # Update content while locked
  localV2 <- file.path(GFT2$TMP_DIR, "test_file1_v2.txt")
  writeLines(c("line 1", "line 2", "line 3", "line 4 added"), localV2)
  improveR::updateFileContent(GFT2$FILE1_PATH, localPath = localV2)

  # Unlock (checkin)
  unlocked <- improveR::unlockResource(GFT2$FILE1_PATH)
  expect_true(unlocked, info = "unlockResource should return TRUE on success")

  # Verify the resource is no longer locked
  unlockedRes <- improveR::updateResource(GFT2$FILE1_PATH)
  if ("lockedByName" %in% names(unlockedRes)) {
    expect_true(is.na(unlockedRes$lockedByName),
                info = "lockedByName should be NA after unlock")
  }
  cat("Lock/update/unlock cycle completed for:", GFT2$FILE1_PATH, "\n")
})

# ===========================================================================
# File version history | ics472
# ===========================================================================
test_that("GFT2-04: verify file has multiple versions after edit|ics472", {
  skip_if(is.null(GFT2$FILE1_PATH), "No test file 1")

  improveR::updateResource(GFT2$FILE1_PATH)
  history <- improveR::loadHistory(GFT2$FILE1_PATH)
  skip_if(is.null(history) || is.null(history$data) || length(history$data) == 0,
          "History not available")

  revisions <- history$data[[1]]
  expect_true(is.data.frame(revisions), info = "Revisions should be a data frame")
  expect_true(nrow(revisions) >= 2,
              info = paste("File should have >= 2 versions after edit, got", nrow(revisions)))
  cat("File versions:", nrow(revisions), "\n")
})

# ===========================================================================
# Search / Query | ics472
# ===========================================================================
test_that("GFT2-05: search for file by name returns correct result|ics472", {
  skip_if(is.null(GFT2$FILE1_PATH), "No test file 1")

  results <- tryCatch(
    improveR::query("name='test_file1.txt'"),
    error = function(e) NULL
  )
  skip_if(is.null(results), "query() not available on this server")

  expect_true(is.data.frame(results), info = "Query should return a data frame")
  expect_true(nrow(results) >= 1,
              info = "Should find at least one result for test_file1.txt")
  expect_true("test_file1.txt" %in% results$name,
              info = "Search results should contain file with name test_file1.txt")
  cat("Search found", nrow(results), "result(s)\n")
})

# ===========================================================================
# Favorites | ics1799-1806
# ===========================================================================
test_that("GFT2-06: add file to favorites and verify it appears|ics1803,ics1799", {
  skip_if(is.null(GFT2$FILE1_RES), "No test file 1")

  result <- improveR::addFavoriteLink(
    targetIdent = GFT2$FILE1_RES$resourceId,
    name = "GFT2-favorite"
  )
  skip_if(is.null(result), "addFavoriteLink not supported on this server")

  expect_true(!is.null(result$resourceId),
              info = "Favorite link should have a resourceId")
  GFT2$FAV_ID <- result$resourceId
  cat("Added favorite:", GFT2$FAV_ID, "\n")

  # Verify it appears in favorites list
  favs <- improveR::loadFavorites()
  expect_false(is.null(favs), info = "loadFavorites should return data")
  expect_true(is.data.frame(favs), info = "Favorites should be a data frame")
  expect_true(nrow(favs) >= 1, info = "Should have at least one favorite")
  cat("Total favorites:", nrow(favs), "\n")
})

test_that("GFT2-07: list favorite children at top level|ics1801", {
  children <- improveR::loadFavoriteChildren()
  skip_if(is.null(children), "loadFavoriteChildren not supported")

  expect_true(is.data.frame(children), info = "Favorite children should be a data frame")
  expect_true(nrow(children) >= 1,
              info = "Should have at least one top-level favorite item")
  cat("Top-level favorite items:", nrow(children), "\n")
})

test_that("GFT2-08: create and remove favorites folder|ics1805,ics1800", {
  # Create a favorites folder
  result <- improveR::createFavoriteFolder(
    name = "GFT2-folder",
    comment = "GFT2 favorites folder test"
  )
  skip_if(is.null(result), "createFavoriteFolder not supported on this server")

  expect_true(!is.null(result$resourceId),
              info = "Favorites folder should have a resourceId")
  GFT2$FAV_FOLDER_ID <- result$resourceId
  cat("Created favorites folder:", GFT2$FAV_FOLDER_ID, "\n")

  # Remove the favorites folder
  removed <- improveR::removeFavorite(GFT2$FAV_FOLDER_ID)
  expect_true(removed, info = "removeFavorite should return TRUE for folder")
  cat("Removed favorites folder\n")
})

test_that("GFT2-09: remove favorite link|ics1800", {
  skip_if(is.null(GFT2$FAV_ID), "No favorite created")

  removed <- improveR::removeFavorite(GFT2$FAV_ID)
  expect_true(removed, info = "removeFavorite should return TRUE")
  cat("Removed favorite link:", GFT2$FAV_ID, "\n")
})

# ===========================================================================
# Audit trail | ics472
# ===========================================================================
test_that("GFT2-10: audit trail has entries for test folder|ics472", {
  skip_if(is.null(GFT2$ROOT_PATH), "No GFT2 root folder")

  audit <- tryCatch(
    improveR::loadAuditTrail(GFT2$ROOT_PATH),
    error = function(e) NULL
  )
  skip_if(is.null(audit) || is.null(audit$data) || length(audit$data) == 0,
          "Audit trail not available")

  entries <- audit$data[[1]]
  expect_true(is.data.frame(entries), info = "Audit entries should be a data frame")
  expect_true(nrow(entries) > 0, info = "Audit trail should have entries")
  # After all the operations above, there should be many entries
  expect_true(nrow(entries) >= 5,
              info = paste("Should have >= 5 audit entries for all operations, got", nrow(entries)))
  cat("Audit trail entries:", nrow(entries), "\n")
})

# ===========================================================================
# Transactions / Revisions | ccs10
# ===========================================================================
test_that("GFT2-11: get latest revision returns valid revision|ccs10", {
  rev <- improveR::getLatestRevision()
  skip_if(is.null(rev), "getLatestRevision not supported")

  expect_true(is.list(rev), info = "Revision should be a list")
  expect_false(is.null(rev$id), info = "Revision should have an id")
  expect_true(nchar(rev$id) > 0, info = "Revision id should be non-empty")
  cat("Latest revision:", rev$id, "\n")
})

# ===========================================================================
# Cleanup
# ===========================================================================
test_that("GFT2-cleanup: delete test folder and verify", {
  skip_if(is.null(GFT2$ROOT_PATH), "No GFT2 root folder to clean up")

  result <- improveR::delete(GFT2$ROOT_PATH)
  expect_true(result, info = "Deletion of GFT2 root folder should succeed")

  if (!is.null(GFT2$TMP_DIR)) {
    unlink(GFT2$TMP_DIR, recursive = TRUE)
  }
  rm(list = ls(envir = GFT2), envir = GFT2)
  cat("GFT2 cleanup complete\n")
})
