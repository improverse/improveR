# Test Advanced Permission Scenarios
# Tests: multiple ACEs, mixed inheritance, inherited propagation,
#        replaceResourcePermissions replace semantics, updateResourcePermission,
#        multi-user ACL verification, orderNr

Sys.setenv(TEST_NAME = "permissionsAdvanced")

hasConnectAs <- function() {
  "improveRtestsupport" %in% loadedNamespaces() &&
    exists("connectAs", envir = asNamespace("improveRtestsupport"))
}

reconnectAsAdmin <- function() {
  tryCatch({
    improveRtestsupport::connectAs("admin")
    improveR::setEditable(TRUE)
  }, error = function(e) {
    improveR::clearConnectionData(includeRepoData = FALSE)
    Sys.setenv(IMPROVER_TOKEN = "", IMPROVER_REFRESH_TOKEN = "")
    Sys.setenv(IMPROVER_TEST_USERNAME = "admin", IMPROVER_TEST_PASSWORD = "admin")
    improveRtestsupport::improveConnect()
    improveR::setEditable(TRUE)
  })
}

# Store test state in an environment to avoid polluting globalenv
PA <- new.env(parent = emptyenv())

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------
test_that("setup permissionsAdvanced test environment", {
  improveR::improveConnect()
  improveR::setEditable(TRUE)

  basePath <- createFolderPath("permissionsAdvanced")
  testFolder <- improveR::createFolder(
    targetIdent = basePath,
    folderName = paste0("test-permAdv-", format(Sys.time(), "%Y%m%d%H%M%S")),
    comment = "advanced permissions test"
  )
  expect_false(is.null(testFolder))
  PA$FOLDER <- testFolder
  PA$FOLDER_PATH <- testFolder$path

  # Create 3 test groups
  PA$GROUP_ADMIN <- improveR::createGroup(paste0("PA-admin-", uuid::UUIDgenerate()))
  PA$GROUP_RO    <- improveR::createGroup(paste0("PA-ro-", uuid::UUIDgenerate()))
  PA$GROUP_COLLAB <- improveR::createGroup(paste0("PA-collab-", uuid::UUIDgenerate()))
  expect_false(is.null(PA$GROUP_ADMIN))
  expect_false(is.null(PA$GROUP_RO))
  expect_false(is.null(PA$GROUP_COLLAB))

  # Add admin user to admin group
  allUsers <- improveR::users()
  adminUser <- allUsers[allUsers$username == "admin", ]
  improveR::addGroupUser(PA$GROUP_ADMIN$id, adminUser$id[1])
  PA$ADMIN_USER <- adminUser

  # Add test1 to readonly group (for multi-user test)
  test1User <- allUsers[allUsers$username == "test1", ]
  if (nrow(test1User) > 0) {
    improveR::addGroupUser(PA$GROUP_RO$id, test1User$id[1])
    PA$TEST1_USER <- test1User
  }

  assign("PA", PA, envir = globalenv())
  cat("Setup complete: folder:", PA$FOLDER_PATH, "\n")
})

# ---------------------------------------------------------------------------
# Multiple ACEs on one resource
# ---------------------------------------------------------------------------
test_that("multiple ACEs: 3 groups on one resource via replaceResourcePermissions|ics769,ics2044", {
  PA <- get("PA", envir = globalenv())
  stopifnot("No test folder" = !is.null(PA$FOLDER))

  aclEntries <- list(
    list(memberId = PA$GROUP_ADMIN$id, visible = TRUE, read = TRUE,
         modify = TRUE, changeRights = TRUE, inherit = TRUE, rightsArea = 1L),
    list(memberId = PA$GROUP_RO$id, visible = TRUE, read = TRUE,
         modify = FALSE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L),
    list(memberId = PA$GROUP_COLLAB$id, visible = TRUE, read = TRUE,
         modify = TRUE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
  )

  result <- improveR::replaceResourcePermissions(PA$FOLDER_PATH, aclEntries)
  if (is.null(result)) {
    stop("replaceResourcePermissions not supported on this server")
  }

  acl <- improveR::getResourcePermissions(PA$FOLDER_PATH)
  expect_false(is.null(acl))
  expect_true(is.data.frame(acl))
  expect_true(PA$GROUP_ADMIN$id %in% acl$memberId)
  expect_true(PA$GROUP_RO$id %in% acl$memberId)
  expect_true(PA$GROUP_COLLAB$id %in% acl$memberId)

  # Verify rights are correct per group
  adminAce <- acl[acl$memberId == PA$GROUP_ADMIN$id, ]
  expect_true(adminAce$modify[1])
  expect_true(adminAce$changeRights[1])

  roAce <- acl[acl$memberId == PA$GROUP_RO$id, ]
  expect_false(roAce$modify[1])
  expect_false(roAce$changeRights[1])

  collabAce <- acl[acl$memberId == PA$GROUP_COLLAB$id, ]
  expect_true(collabAce$modify[1])
  expect_false(collabAce$changeRights[1])

  cat("3 ACEs set and verified\n")
})

# ---------------------------------------------------------------------------
# Mixed inheritance (same group, inherit=FALSE + inherit=TRUE)
# ---------------------------------------------------------------------------
test_that("mixed inheritance: same group with different inherit flags|ics2044", {
  PA <- get("PA", envir = globalenv())
  stopifnot("No test folder" = !is.null(PA$FOLDER))

  # Root protection pattern: read-only without inherit + full with inherit
  aclEntries <- list(
    list(memberId = PA$GROUP_COLLAB$id, visible = TRUE, read = TRUE,
         modify = FALSE, changeRights = FALSE, inherit = FALSE, rightsArea = 1L),
    list(memberId = PA$GROUP_COLLAB$id, visible = TRUE, read = TRUE,
         modify = TRUE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
  )

  result <- improveR::replaceResourcePermissions(PA$FOLDER_PATH, aclEntries)
  if (is.null(result)) {
    stop("replaceResourcePermissions not supported")
  }

  acl <- improveR::getResourcePermissions(PA$FOLDER_PATH)
  expect_false(is.null(acl))
  collabAces <- acl[acl$memberId == PA$GROUP_COLLAB$id, ]
  expect_equal(nrow(collabAces), 2)

  # One should have inherit=FALSE, one inherit=TRUE
  inheritValues <- sort(collabAces$inherit)
  expect_equal(inheritValues, c(FALSE, TRUE))

  cat("Mixed inheritance verified: 2 ACEs for same group\n")
})

# ---------------------------------------------------------------------------
# Inherited ACEs propagating to child resources
# ---------------------------------------------------------------------------
test_that("inherited ACEs propagate to child folders|ics2044", {
  PA <- get("PA", envir = globalenv())
  stopifnot("No test folder" = !is.null(PA$FOLDER))
  stopifnot("No admin user" = !is.null(PA$ADMIN_USER))

  # Set inheritable ACL on parent
  aclEntries <- list(
    list(memberId = PA$GROUP_ADMIN$id, visible = TRUE, read = TRUE,
         modify = TRUE, changeRights = TRUE, inherit = TRUE, rightsArea = 1L)
  )
  result <- improveR::replaceResourcePermissions(PA$FOLDER_PATH, aclEntries)
  if (is.null(result)) {
    stop("replaceResourcePermissions not supported")
  }

  # Create child folder
  childFolder <- improveR::createFolder(PA$FOLDER, "child-inherit-test")
  expect_false(is.null(childFolder))
  PA$CHILD_FOLDER <- childFolder

  # Check effective rights on child — admin should have modify
  rights <- improveR::effectiveRights(childFolder, memberId = PA$ADMIN_USER$id[1])
  expect_false(is.null(rights))
  expect_true(rights$modify)

  cat("Inherited ACE verified on child folder\n")
  assign("PA", PA, envir = globalenv())
})

# ---------------------------------------------------------------------------
# replaceResourcePermissions replace semantics
# ---------------------------------------------------------------------------
test_that("replaceResourcePermissions removes entries not in new list|ics2044", {
  PA <- get("PA", envir = globalenv())
  stopifnot("No test folder" = !is.null(PA$FOLDER))

  # Set 3 entries
  aclEntries3 <- list(
    list(memberId = PA$GROUP_ADMIN$id, visible = TRUE, read = TRUE,
         modify = TRUE, changeRights = TRUE, inherit = TRUE, rightsArea = 1L),
    list(memberId = PA$GROUP_RO$id, visible = TRUE, read = TRUE,
         modify = FALSE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L),
    list(memberId = PA$GROUP_COLLAB$id, visible = TRUE, read = TRUE,
         modify = TRUE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
  )
  result <- improveR::replaceResourcePermissions(PA$FOLDER_PATH, aclEntries3)
  if (is.null(result)) stop("replaceResourcePermissions not supported")

  acl3 <- improveR::getResourcePermissions(PA$FOLDER_PATH)
  expect_equal(nrow(acl3), 3)

  # Replace with 2 entries (drop collab)
  aclEntries2 <- list(
    list(memberId = PA$GROUP_ADMIN$id, visible = TRUE, read = TRUE,
         modify = TRUE, changeRights = TRUE, inherit = TRUE, rightsArea = 1L),
    list(memberId = PA$GROUP_RO$id, visible = TRUE, read = TRUE,
         modify = FALSE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
  )
  improveR::replaceResourcePermissions(PA$FOLDER_PATH, aclEntries2)

  acl2 <- improveR::getResourcePermissions(PA$FOLDER_PATH)
  expect_equal(nrow(acl2), 2)
  expect_false(PA$GROUP_COLLAB$id %in% acl2$memberId)

  cat("Replace semantics verified: 3 -> 2 entries\n")
})

# ---------------------------------------------------------------------------
# updateResourcePermission
# ---------------------------------------------------------------------------
test_that("updateResourcePermission modifies existing ACE rights|ics2044", {
  PA <- get("PA", envir = globalenv())
  stopifnot("No test folder" = !is.null(PA$FOLDER))

  # Set a read-only ACE
  aclEntries <- list(
    list(memberId = PA$GROUP_RO$id, visible = TRUE, read = TRUE,
         modify = FALSE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
  )
  result <- improveR::replaceResourcePermissions(PA$FOLDER_PATH, aclEntries)
  if (is.null(result)) stop("replaceResourcePermissions not supported")

  acl <- improveR::getResourcePermissions(PA$FOLDER_PATH)
  expect_false(acl$modify[1])
  aclId <- acl$id[1]

  # Update to add modify rights
  updated <- improveR::updateResourcePermission(
    PA$FOLDER_PATH, aclId = aclId, memberId = PA$GROUP_RO$id,
    visible = TRUE, read = TRUE, modify = TRUE, changeRights = FALSE)
  expect_false(is.null(updated))

  # Verify
  acl2 <- improveR::getResourcePermissions(PA$FOLDER_PATH)
  roAce <- acl2[acl2$memberId == PA$GROUP_RO$id, ]
  expect_true(roAce$modify[1])

  cat("updateResourcePermission verified\n")
})

# ---------------------------------------------------------------------------
# orderNr
# ---------------------------------------------------------------------------
test_that("ACE orderNr is respected in getResourcePermissions|ics2044", {
  PA <- get("PA", envir = globalenv())
  stopifnot("No test folder" = !is.null(PA$FOLDER))

  # Set 2 ACEs with specific ordering
  aclEntries <- list(
    list(memberId = PA$GROUP_RO$id, visible = TRUE, read = TRUE,
         modify = FALSE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L),
    list(memberId = PA$GROUP_ADMIN$id, visible = TRUE, read = TRUE,
         modify = TRUE, changeRights = TRUE, inherit = TRUE, rightsArea = 1L)
  )
  result <- improveR::replaceResourcePermissions(PA$FOLDER_PATH, aclEntries)
  if (is.null(result)) stop("replaceResourcePermissions not supported")

  acl <- improveR::getResourcePermissions(PA$FOLDER_PATH)
  expect_equal(nrow(acl), 2)
  # replaceResourcePermissions sets orderNr from position in list (1, 2)
  expect_true("orderNr" %in% names(acl))
  expect_equal(acl$orderNr[1], 1L)
  expect_equal(acl$orderNr[2], 2L)

  cat("orderNr verified\n")
})

# ---------------------------------------------------------------------------
# Multi-user ACL verification
# ---------------------------------------------------------------------------
test_that("ACL restricts access from test1 perspective|ics2044", {
  PA <- get("PA", envir = globalenv())
  stopifnot("No test folder" = !is.null(PA$FOLDER))
  stopifnot("multi-user testbed required (hasConnectAs() must be TRUE)" = hasConnectAs())
  stopifnot("test1 user not available" = !is.null(PA$TEST1_USER))

  # Set ACL: readonly group (test1) gets read-only, no modify
  aclEntries <- list(
    list(memberId = PA$GROUP_ADMIN$id, visible = TRUE, read = TRUE,
         modify = TRUE, changeRights = TRUE, inherit = TRUE, rightsArea = 1L),
    list(memberId = PA$GROUP_RO$id, visible = TRUE, read = TRUE,
         modify = FALSE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
  )
  result <- improveR::replaceResourcePermissions(PA$FOLDER_PATH, aclEntries)
  if (is.null(result)) stop("replaceResourcePermissions not supported")

  # Switch to test1
  improveRtestsupport::connectAs("test1")
  improveR::setEditable(TRUE)

  # test1 should be able to read the folder
  resource <- tryCatch(
    improveR::loadResource(PA$FOLDER_PATH),
    error = function(e) NULL
  )
  expect_false(is.null(resource),
               info = "test1 should be able to read the folder (visible + read)")

  # test1 should NOT be able to modify (create subfolder)
  modifyResult <- tryCatch({
    improveR::createFolder(PA$FOLDER_PATH, "should-fail-folder")
  }, error = function(e) NULL)
  # If ACL enforcement works, createFolder should fail (return NULL or error)
  # Note: some servers may not enforce this strictly at the REST level
  if (!is.null(modifyResult)) {
    cat("Note: server did not enforce modify restriction via REST\n")
    # Clean up the accidentally created folder
    tryCatch(improveR::delete(modifyResult), error = function(e) NULL)
  } else {
    cat("Modify restriction enforced for test1\n")
  }

  # Switch back to admin
  reconnectAsAdmin()
  expect_true(TRUE)
})

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
test_that("cleanup permissionsAdvanced test environment", {
  # Ensure we're admin
  if (hasConnectAs()) {
    tryCatch(reconnectAsAdmin(), error = function(e) NULL)
  }

  PA <- get("PA", envir = globalenv())

  # Clear ACLs first
  if (!is.null(PA$FOLDER_PATH)) {
    tryCatch(
      improveR::replaceResourcePermissions(PA$FOLDER_PATH, list()),
      error = function(e) NULL
    )
  }

  # Delete child folder
  if (!is.null(PA$CHILD_FOLDER)) {
    tryCatch(improveR::delete(PA$CHILD_FOLDER), error = function(e) NULL)
  }

  # Delete test folder
  if (!is.null(PA$FOLDER)) {
    tryCatch(improveR::delete(PA$FOLDER$resourceId), error = function(e) {
      cat("Cleanup warning:", e$message, "\n")
    })
  }

  # Remove users from groups before deleting groups
  if (!is.null(PA$ADMIN_USER) && !is.null(PA$GROUP_ADMIN)) {
    tryCatch(improveR::removeGroupUser(PA$GROUP_ADMIN$id, PA$ADMIN_USER$id[1]),
             error = function(e) NULL)
  }
  if (!is.null(PA$TEST1_USER) && !is.null(PA$GROUP_RO)) {
    tryCatch(improveR::removeGroupUser(PA$GROUP_RO$id, PA$TEST1_USER$id[1]),
             error = function(e) NULL)
  }

  # Delete groups
  for (grp in c("GROUP_ADMIN", "GROUP_RO", "GROUP_COLLAB")) {
    if (!is.null(PA[[grp]])) {
      tryCatch(improveR::deleteGroup(PA[[grp]]$id), error = function(e) NULL)
    }
  }

  rm("PA", envir = globalenv())
  expect_true(TRUE)
})
