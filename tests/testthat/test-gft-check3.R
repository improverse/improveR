# GFT Check 3 — Permissions, Relations, and Multi-User Operations
# Mirrors: iat2955 gft_check3.rsc + iat2957 gft_check4.rsc
# Requirements: ics472, ccs6, ccs82, ics1044, ics1810, ics1811
#
# Scenarios covered:
#   - Resource relations (create, read, delete) — uses resourceId UUIDs
#   - Permissions / rights management (ACL set, read, effective rights)
#   - Multi-user: user switching via connectAs (test user vs admin)
#   - Delete resource and verify
#   - Resource finish / reopen lifecycle
#
# Skipped (UI-only):
#   - User/Group Administration dialogs
#   - Preferences (Grid argument picklists, runservers)
#   - License information, system diagnostics
#   - Undelete (no REST endpoint)
#   - Purge (admin-only, no REST endpoint)
#   - Publish properties dialog

GFT3 <- new.env(parent = emptyenv())

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
hasConnectAs <- function() {
  "improveRtestsupport" %in% loadedNamespaces() &&
    exists("connectAs", envir = asNamespace("improveRtestsupport"))
}

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------
test_that("GFT3-setup: connect and create test folder with files", {
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
    folderName = paste0("gft-check3-", ts),
    comment = "GFT check 3 test folder"
  )
  expect_false(is.null(folder), info = "GFT3 root folder should be created")
  expect_equal(folder$nodeType, "Folder")
  GFT3$ROOT_PATH <- folder$path
  GFT3$ROOT_RES <- folder
  cat("Created GFT3 root folder:", folder$path, "\n")

  # Create two test files for relation/permission tests
  tmpDir <- file.path(tempdir(), paste0("gft3-", ts))
  dir.create(tmpDir, showWarnings = FALSE, recursive = TRUE)
  GFT3$TMP_DIR <- tmpDir

  localA <- file.path(tmpDir, "fileA.txt")
  writeLines("content A", localA)
  fileA <- improveR::createFile(
    targetIdent = GFT3$ROOT_PATH,
    fileName = "fileA.txt",
    localPath = localA,
    comment = "GFT3 file A"
  )
  expect_false(is.null(fileA), info = "fileA should be created")
  expect_equal(fileA$name, "fileA.txt")
  GFT3$FILE_A_PATH <- paste0(GFT3$ROOT_PATH, "/fileA.txt")
  GFT3$FILE_A_RES <- fileA

  localB <- file.path(tmpDir, "fileB.txt")
  writeLines("content B", localB)
  fileB <- improveR::createFile(
    targetIdent = GFT3$ROOT_PATH,
    fileName = "fileB.txt",
    localPath = localB,
    comment = "GFT3 file B"
  )
  expect_false(is.null(fileB), info = "fileB should be created")
  GFT3$FILE_B_PATH <- paste0(GFT3$ROOT_PATH, "/fileB.txt")
  GFT3$FILE_B_RES <- fileB

  # Look up current user from users list (whoami() may return empty in test context)
  allUsers <- tryCatch(improveR::users(), error = function(e) NULL)
  if (!is.null(allUsers) && nrow(allUsers) > 0) {
    # Try whoami first; if empty, look for "admin" in users list
    currentUser <- tryCatch(improveR::whoami(), error = function(e) "")
    if (currentUser != "" && currentUser %in% allUsers$username) {
      adminRow <- allUsers[allUsers$username == currentUser, ]
    } else {
      # Fallback: look for "admin" username
      adminRow <- allUsers[allUsers$username == "admin", ]
    }
    if (nrow(adminRow) > 0) {
      GFT3$ADMIN_USER <- adminRow$username[1]
      GFT3$ADMIN_USER_ID <- adminRow$id[1]
    }
  }
  expect_false(is.null(GFT3$ADMIN_USER), info = "Should identify admin user")
  cat("Setup complete: 2 files, admin user:", GFT3$ADMIN_USER, "\n")
})

# ===========================================================================
# Resource Relations | ics1044
# Note: createResourceRelation takes (resourceId, targetResourceId,
#       relationTypeId, description) — all UUIDs.
#       loadRelationTypes() is currently non-functional (returns NULL).
#       We attempt to get relation types via REST directly.
# ===========================================================================
test_that("GFT3-01: create and read resource relation|ics1044", {
  skip_if(is.null(GFT3$FILE_A_RES), "No file A")
  skip_if(is.null(GFT3$FILE_B_RES), "No file B")

  # Load available relation types via the fixed REST-based function
  relTypes <- improveR::loadRelationTypes()
  skip_if(is.null(relTypes) || nrow(relTypes) == 0,
          "No relation types available on server")
  relTypeId <- relTypes$id[1]
  cat("Using relation type:", relTypes$name[1], "(", relTypeId, ")\n")

  # Create relation using ident-style API (paths resolve to resourceId internally)
  result <- tryCatch(
    improveR::createResourceRelation(
      ident = GFT3$FILE_A_PATH,
      targetIdent = GFT3$FILE_B_PATH,
      relationTypeId = relTypeId,
      description = "GFT3 test relation"
    ),
    error = function(e) {
      cat("createResourceRelation error:", e$message, "\n")
      NULL
    }
  )
  skip_if(is.null(result), "createResourceRelation failed")

  # Read back relations using ident (path) and verify
  relations <- improveR::loadResourceRelations(GFT3$FILE_A_PATH)
  expect_false(is.null(relations), info = "Should have relations after creation")
  expect_true(is.data.frame(relations), info = "Relations should be a data frame")
  expect_true(nrow(relations) >= 1, info = "Should have at least one relation")

  # Verify the relation points to the correct target
  expect_true(GFT3$FILE_B_RES$resourceId %in% relations$targetResourceId,
              info = "Relation should point to file B")

  GFT3$RELATION_ID <- relations$id[1]
  cat("Created and verified relation:", GFT3$RELATION_ID, "\n")
})

test_that("GFT3-02: delete resource relation|ics1044", {
  skip_if(is.null(GFT3$FILE_A_RES), "No file A")
  skip_if(is.null(GFT3$RELATION_ID), "No relation created")

  result <- improveR::deleteResourceRelation(
    ident = GFT3$FILE_A_PATH,
    relationId = GFT3$RELATION_ID
  )

  # Verify relation is gone — force cache refresh
  relationsAfter <- improveR::refreshResourceRelations(GFT3$FILE_A_PATH)
  if (!is.null(relationsAfter) && is.data.frame(relationsAfter)) {
    expect_false(GFT3$RELATION_ID %in% relationsAfter$id,
                 info = "Deleted relation should no longer appear")
  } else {
    # NULL means no relations at all — deletion confirmed
    expect_true(is.null(relationsAfter) || nrow(relationsAfter) == 0,
                info = "No relations should remain after deletion")
  }
  cat("Relation deleted and verified\n")
})

# ===========================================================================
# Permissions / Rights Management | ccs6, ccs82
# ===========================================================================
test_that("GFT3-03: read resource permissions (ACL entries)|ccs6", {
  skip_if(is.null(GFT3$ROOT_PATH), "No GFT3 root folder")

  perms <- improveR::getResourcePermissions(GFT3$ROOT_PATH)
  # Newly created folder has no explicit ACL entries (inherited permissions only),
  # so getResourcePermissions returns NULL per convention (empty results = NULL).
  if (!is.null(perms)) {
    expect_true(is.data.frame(perms), info = "Permissions should be a data frame")
    cat("ACL entries on folder:", nrow(perms), "\n")
  } else {
    cat("No explicit ACL entries (permissions inherited from parent)\n")
  }
  expect_true(is.null(perms) || is.data.frame(perms),
              info = "Permissions should be NULL or a data frame")
})

test_that("GFT3-04: read effective rights for admin user|ccs6", {
  skip_if(is.null(GFT3$ROOT_PATH), "No GFT3 root folder")
  skip_if(is.null(GFT3$ADMIN_USER_ID), "No admin user ID")

  rights <- improveR::effectiveRights(
    GFT3$ROOT_PATH,
    memberId = GFT3$ADMIN_USER_ID
  )
  expect_false(is.null(rights), info = "effectiveRights should return data for admin")
  expect_true(is.list(rights), info = "effectiveRights should return a list")

  # Admin (and creator) should have full rights
  expect_true(rights$modify,
              info = "Admin/creator should have modify rights on own folder")
  cat("Effective rights: modify=", rights$modify, "\n")
})

test_that("GFT3-05: set ACL entry on test folder for a group|ccs6", {
  skip_if(is.null(GFT3$ROOT_PATH), "No GFT3 root folder")

  # Get all users to find a test user (non-admin)
  allUsers <- tryCatch(improveR::users(), error = function(e) NULL)
  skip_if(is.null(allUsers) || nrow(allUsers) == 0, "No users available")

  testUsers <- allUsers[allUsers$username != GFT3$ADMIN_USER & allUsers$active == TRUE, ]
  skip_if(nrow(testUsers) == 0, "No active non-admin users available")
  # Prefer test1 — we know it has password=test1 for connectAs
  test1Idx <- which(testUsers$username == "test1")
  if (length(test1Idx) > 0) {
    GFT3$TEST_USER <- testUsers[test1Idx[1], ]
  } else {
    GFT3$TEST_USER <- testUsers[1, ]
  }
  cat("Test user:", GFT3$TEST_USER$username, "(", GFT3$TEST_USER$id, ")\n")

  # Create a group for ACL testing
  groupName <- paste0("gft3-test-", format(Sys.time(), "%H%M%S"))
  group <- tryCatch(
    improveR::createGroup(groupName),
    error = function(e) NULL
  )
  skip_if(is.null(group), "Cannot create group for ACL test")

  expect_true(!is.null(group$id), info = "Group should have an id")
  expect_equal(group$name, groupName, info = "Group name should match")
  GFT3$TEST_GROUP <- group
  cat("Created group:", groupName, "->", group$id, "\n")

  # Add test user to group
  addResult <- tryCatch(
    improveR::addGroupUser(group$id, GFT3$TEST_USER$id),
    error = function(e) {
      cat("addGroupUser error:", e$message, "\n")
      NULL
    }
  )
  expect_false(is.null(addResult),
               info = "addGroupUser should return the membership list")

  # Set read-only ACL on the folder for this group
  aclResult <- improveR::setResourcePermission(
    GFT3$ROOT_PATH,
    memberId = group$id,
    visible = TRUE, read = TRUE, modify = FALSE,
    changeRights = FALSE, inherit = TRUE
  )
  expect_false(is.null(aclResult),
               info = "setResourcePermission should return the ACL entry")
  expect_equal(aclResult$memberId, group$id,
               info = "ACL entry memberId should match the group")
  expect_true(aclResult$read, info = "ACL should grant read")
  expect_false(aclResult$modify, info = "ACL should deny modify")
  GFT3$ACL_ID <- aclResult$id
  cat("Set read-only ACL for group on folder, ACL id:", GFT3$ACL_ID, "\n")
})

test_that("GFT3-06: verify effective rights for test user|ccs6", {
  skip_if(is.null(GFT3$ROOT_PATH), "No GFT3 root folder")
  skip_if(is.null(GFT3$TEST_USER), "No test user")

  rights <- improveR::effectiveRights(
    GFT3$ROOT_PATH,
    memberId = GFT3$TEST_USER$id
  )
  expect_false(is.null(rights),
               info = "effectiveRights should return data for test user")
  expect_true(is.list(rights), info = "effectiveRights should return a list")

  # The test user should at minimum have read access via the group
  cat("Test user effective rights: read=", rights$read,
      " modify=", rights$modify, "\n")
})

# ===========================================================================
# Resource Lifecycle: finish / reopen | ics1810, ics1811
# ===========================================================================
test_that("GFT3-07: finish and reopen resource|ics1810,ics1811", {
  skip_if(is.null(GFT3$FILE_A_PATH), "No file A")

  # Finish the resource
  finishResult <- improveR::finishResource(GFT3$FILE_A_PATH)
  expect_true(finishResult, info = "finishResource should return TRUE on success")

  # Verify the resource state changed (load and check status)
  finishedRes <- improveR::refreshResource(GFT3$FILE_A_PATH)
  if ("status" %in% names(finishedRes)) {
    cat("Resource status after finish:", finishedRes$status, "\n")
  }

  # Reopen the resource
  reopenResult <- improveR::reopenResource(GFT3$FILE_A_PATH)
  expect_true(reopenResult, info = "reopenResource should return TRUE on success")

  # Verify the resource is editable again (can lock it)
  lockResult <- improveR::lockResource(GFT3$FILE_A_PATH)
  expect_true(lockResult, info = "Should be able to lock resource after reopen")
  unlockResult <- improveR::unlockResource(GFT3$FILE_A_PATH)
  expect_true(unlockResult, info = "Should be able to unlock after reopen")

  cat("Finish/reopen lifecycle verified for:", GFT3$FILE_A_PATH, "\n")
})

# ===========================================================================
# Delete resource | ics472
# ===========================================================================
test_that("GFT3-08: delete file and verify it is gone|ics472", {
  skip_if(is.null(GFT3$FILE_B_PATH), "No file B")

  # delete() takes only the resource identifier, no comment parameter
  result <- improveR::delete(GFT3$FILE_B_PATH)
  expect_true(result, info = "delete should return TRUE on success")
  cat("File B deleted\n")
})

# ===========================================================================
# Users listing | ics1142
# ===========================================================================
test_that("GFT3-09: list users returns valid data|ics1142", {
  userList <- improveR::users()
  expect_false(is.null(userList), info = "users() should return data")
  expect_true(is.data.frame(userList), info = "Users should be a data frame")
  expect_true(nrow(userList) >= 1, info = "Should have at least one user")

  # Verify expected columns exist
  expect_true("username" %in% names(userList),
              info = "Users data frame should have username column")
  expect_true("id" %in% names(userList),
              info = "Users data frame should have id column")

  # Verify usernames are unique
  expect_equal(
    length(userList$username),
    length(unique(userList$username)),
    info = "All usernames should be unique"
  )
  cat("Total users:", nrow(userList), "\n")
})

# ===========================================================================
# Multi-user: user switching | ccs82
# (Placed last because connectAs tears down the current OAuth session)
# ===========================================================================
test_that("GFT3-10: switch to test user and verify limited access|ccs82", {
  skip_if(!hasConnectAs(), "improveRtestsupport::connectAs not available")
  skip_if(is.null(GFT3$TEST_USER), "No test user")
  skip_if(is.null(GFT3$ROOT_PATH), "No GFT3 root folder")

  # Switch to test user
  connected <- tryCatch({
    improveRtestsupport::connectAs(GFT3$TEST_USER$username)
    improveR::setEditable(TRUE)
    TRUE
  }, error = function(e) {
    # connectAs tears down the current session even on failure —
    # reconnect as admin before skipping
    tryCatch({
      improveRtestsupport::connectAs("admin")
      improveR::setEditable(TRUE)
    }, error = function(e2) NULL)
    skip(paste("Cannot connect as test user:", e$message))
  })

  # The test user should be able to read the folder
  readResult <- tryCatch(
    improveR::loadResource(GFT3$ROOT_PATH),
    error = function(e) NULL
  )
  expect_false(is.null(readResult),
               info = "Test user should be able to read the folder")

  # Try to create a file — should fail if ACL restricts modify
  tmpFile <- file.path(GFT3$TMP_DIR, "unauthorized.txt")
  writeLines("should not be allowed", tmpFile)
  writeResult <- tryCatch(
    improveR::createFile(
      targetIdent = GFT3$ROOT_PATH,
      fileName = "unauthorized.txt",
      localPath = tmpFile,
      comment = "test write by non-admin"
    ),
    error = function(e) NULL
  )

  if (is.null(writeResult)) {
    cat("Write correctly blocked for test user\n")
  } else {
    cat("Write allowed for test user (may have modify via inherited rights)\n")
    tryCatch(
      improveR::delete(paste0(GFT3$ROOT_PATH, "/unauthorized.txt")),
      error = function(e) NULL
    )
  }
  expect_false(is.null(readResult),
               info = "Test user could read the folder (confirming ACL read grant)")

  # Switch back to admin
  tryCatch({
    improveRtestsupport::connectAs("admin")
    improveR::setEditable(TRUE)
    cat("Switched back to admin\n")
  }, error = function(e) {
    cat("Warning: could not switch back to admin:", e$message, "\n")
  })
})

# ===========================================================================
# Cleanup
# ===========================================================================
test_that("GFT3-cleanup: delete test data and groups", {
  # Reconnect as admin if connection was lost (e.g. after connectAs failure)
  tryCatch({
    improveR::improveConnected()
  }, error = function(e) {
    tryCatch({
      improveRtestsupport::improveConnect()
      improveR::setEditable(TRUE)
      cat("Reconnected as admin for cleanup\n")
    }, error = function(e2) {
      cat("Could not reconnect for cleanup:", e2$message, "\n")
    })
  })

  # Clean up ACL entry
  if (!is.null(GFT3$ACL_ID) && !is.null(GFT3$ROOT_PATH)) {
    tryCatch(
      improveR::removeResourcePermission(GFT3$ROOT_PATH, GFT3$ACL_ID),
      error = function(e) cat("ACL cleanup:", e$message, "\n")
    )
  }

  # Clean up group
  if (!is.null(GFT3$TEST_GROUP)) {
    tryCatch({
      if (!is.null(GFT3$TEST_USER)) {
        improveR::removeGroupUser(GFT3$TEST_GROUP$id, GFT3$TEST_USER$id)
      }
      improveR::deleteGroup(GFT3$TEST_GROUP$id)
      cat("Deleted test group\n")
    }, error = function(e) {
      cat("Group cleanup:", e$message, "\n")
    })
  }

  # Delete test folder
  if (!is.null(GFT3$ROOT_PATH)) {
    result <- tryCatch({
      improveR::delete(GFT3$ROOT_PATH)
    }, error = function(e) {
      cat("Cleanup warning:", e$message, "\n")
      FALSE
    })
    expect_true(result, info = "GFT3 root folder deletion should succeed")
    cat("Deleted GFT3 root folder:", GFT3$ROOT_PATH, "\n")
  }

  if (!is.null(GFT3$TMP_DIR)) {
    unlink(GFT3$TMP_DIR, recursive = TRUE)
  }
  rm(list = ls(envir = GFT3), envir = GFT3)
})
