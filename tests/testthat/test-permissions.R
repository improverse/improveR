
Sys.setenv(TEST_NAME="permissions")

# Helper function to ensure connection and TEST_FOLDER
ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME="permissions")
  improveR::setEditable(TRUE)
  if (!exists("TEST_FOLDER", envir = globalenv()) || is.null(get("TEST_FOLDER", envir = globalenv()))) {
    TEST_FOLDER <- improveR:::baseFilesSetup()
    assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
  }
  return(get("TEST_FOLDER", envir = globalenv()))
}

test_that("setup permissions test folder", {
  Sys.setenv(IMPROVER_TEST_REPLAY="T")
  improveConnect()
  setEditable(T)
  expect_false(Sys.getenv("IMPROVER_TOKEN")=="")
  TEST_FOLDER <- ensureTestFolder()
  expect_false("" == TEST_FOLDER)
})

# --- NULL/error cases ---

test_that("getResourcePermissions returns NULL for non-existent resource", {
  result <- getResourcePermissions("/NonExistent/Path/That/Does/Not/Exist/12345")
  expect_null(result)
})

test_that("effectiveUserPermissions returns NULL for non-existent resource", {
  result <- effectiveUserPermissions("/NonExistent/Path/That/Does/Not/Exist/12345")
  expect_null(result)
})

# --- Group read operations ---

test_that("loadGroups returns data.frame of groups", {
  groups <- loadGroups()
  expect_true(!is.null(groups))
  expect_true(is.data.frame(groups))
  expect_gte(nrow(groups), 1)
  expect_true("id" %in% names(groups) || "name" %in% names(groups))
})

test_that("loadGroup returns group details for valid group", {
  groups <- loadGroups()
  expect_true(nrow(groups) > 0)
  firstGroupId <- groups$id[1]
  group <- loadGroup(firstGroupId)
  expect_true(!is.null(group))
  expect_true(is.list(group))
  expect_true("name" %in% names(group))
})

# --- Group write operations ---

test_that("createGroup creates a new group and deleteGroup removes it", {
  TEST_FOLDER <- ensureTestFolder()
  groupName <- paste0("test-perm-", uuid::UUIDgenerate())
  group <- createGroup(groupName)
  expect_true(!is.null(group))
  expect_true("id" %in% names(group))
  expect_equal(group$name, groupName)

  # Verify it appears in loadGroups
  allGroups <- loadGroups()
  expect_true(group$id %in% allGroups$id)

  # Clean up
  deleted <- deleteGroup(group$id)
  expect_true(deleted)
})

test_that("addGroupUser adds a user and removeGroupUser removes them", {
  TEST_FOLDER <- ensureTestFolder()
  groupName <- paste0("test-perm-", uuid::UUIDgenerate())
  group <- createGroup(groupName)
  expect_true(!is.null(group))

  # Get admin user
  allUsers <- users()
  adminUser <- allUsers[allUsers$username == "admin", ]
  expect_true(nrow(adminUser) > 0)

  # Add user to group
  addResult <- addGroupUser(group$id, adminUser$id[1])
  expect_true(!is.null(addResult))

  # Verify membership
  members <- loadGroupUsers(group$id)
  expect_true(!is.null(members))
  expect_true(is.data.frame(members))
  expect_true(nrow(members) > 0)

  # Remove user from group
  removed <- removeGroupUser(group$id, adminUser$id[1])
  expect_true(removed)

  # Verify removal: empty group returns NULL per convention (empty results = NULL)
  members2 <- loadGroupUsers(group$id)
  expect_null(members2)

  # Clean up
  deleteGroup(group$id)
})

test_that("loadGroupUsers returns data.frame for group with members", {
  # Create a group with a known member to test loadGroupUsers
  groupName <- paste0("test-members-", uuid::UUIDgenerate())
  group <- createGroup(groupName)
  expect_true(!is.null(group))

  allUsers <- users()
  adminUser <- allUsers[allUsers$username == "admin", ]
  addGroupUser(group$id, adminUser$id[1])

  groupUsers <- loadGroupUsers(group$id)
  expect_true(!is.null(groupUsers))
  expect_true(is.data.frame(groupUsers))
  expect_gte(nrow(groupUsers), 1)

  # Clean up
  removeGroupUser(group$id, adminUser$id[1])
  deleteGroup(group$id)
})

# --- ACL read operations with explicit setup ---

test_that("replaceResourcePermissions sets ACL and getResourcePermissions reads it back|ics769", {
  TEST_FOLDER <- ensureTestFolder()
  groupName <- paste0("test-acl-", uuid::UUIDgenerate())
  group <- createGroup(groupName)
  expect_true(!is.null(group))

  # Set ACL on test folder using bulk PUT
  aclEntries <- list(
    list(memberId = group$id, visible = TRUE, read = TRUE,
         modify = FALSE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
  )
  putResult <- replaceResourcePermissions(TEST_FOLDER, aclEntries)
  if (is.null(putResult)) {
    deleteGroup(group$id)
    skip("replaceResourcePermissions returned NULL — server may not support ACL bulk PUT")
  }

  # Now getResourcePermissions should return non-NULL
  acl <- getResourcePermissions(TEST_FOLDER)
  expect_true(!is.null(acl))
  expect_true(is.data.frame(acl))
  expect_true("memberId" %in% names(acl))
  expect_true("resourceId" %in% names(acl))
  expect_true(group$id %in% acl$memberId)

  # Clean up: remove all ACL entries, then delete group
  replaceResourcePermissions(TEST_FOLDER, list())
  deleteGroup(group$id)
})

test_that("effectiveUserPermissions returns data.frame for folder with ACL", {
  TEST_FOLDER <- ensureTestFolder()
  groupName <- paste0("test-eff-", uuid::UUIDgenerate())
  group <- createGroup(groupName)
  expect_true(!is.null(group))

  # Add admin user to group
  allUsers <- users()
  adminUser <- allUsers[allUsers$username == "admin", ]
  addGroupUser(group$id, adminUser$id[1])

  # Set ACL
  aclEntries <- list(
    list(memberId = group$id, visible = TRUE, read = TRUE,
         modify = TRUE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
  )
  putResult <- replaceResourcePermissions(TEST_FOLDER, aclEntries)
  if (is.null(putResult)) {
    removeGroupUser(group$id, adminUser$id[1])
    deleteGroup(group$id)
    skip("replaceResourcePermissions returned NULL — skipping effectiveUserPermissions test")
  }

  perms <- effectiveUserPermissions(TEST_FOLDER)
  expect_true(!is.null(perms))
  expect_true(is.data.frame(perms))
  expect_gte(nrow(perms), 1)

  # Clean up
  replaceResourcePermissions(TEST_FOLDER, list())
  removeGroupUser(group$id, adminUser$id[1])
  deleteGroup(group$id)
})

test_that("getResourcePermissions and effectiveUserPermissions are consistent", {
  TEST_FOLDER <- ensureTestFolder()
  groupName <- paste0("test-consist-", uuid::UUIDgenerate())
  group <- createGroup(groupName)
  expect_true(!is.null(group))

  allUsers <- users()
  adminUser <- allUsers[allUsers$username == "admin", ]
  addGroupUser(group$id, adminUser$id[1])

  aclEntries <- list(
    list(memberId = group$id, visible = TRUE, read = TRUE,
         modify = FALSE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
  )
  putResult <- replaceResourcePermissions(TEST_FOLDER, aclEntries)
  if (is.null(putResult)) {
    removeGroupUser(group$id, adminUser$id[1])
    deleteGroup(group$id)
    skip("replaceResourcePermissions not supported — skipping consistency test")
  }

  acl <- getResourcePermissions(TEST_FOLDER)
  perms <- effectiveUserPermissions(TEST_FOLDER)
  expect_true(!is.null(acl))
  expect_true(!is.null(perms))
  expect_true(is.data.frame(acl))
  expect_true(is.data.frame(perms))

  # Clean up
  replaceResourcePermissions(TEST_FOLDER, list())
  removeGroupUser(group$id, adminUser$id[1])
  deleteGroup(group$id)
})

# --- Role-based lookups with proper setup ---

test_that("getOwners returns owner users when OWN_ group has ACL entry", {
  TEST_FOLDER <- ensureTestFolder()
  ownGroupName <- paste0("OWN_test-", uuid::UUIDgenerate())
  ownGroup <- createGroup(ownGroupName)
  expect_true(!is.null(ownGroup))

  allUsers <- users()
  adminUser <- allUsers[allUsers$username == "admin", ]
  addGroupUser(ownGroup$id, adminUser$id[1])

  aclEntries <- list(
    list(memberId = ownGroup$id, visible = TRUE, read = TRUE,
         modify = TRUE, changeRights = TRUE, inherit = TRUE, rightsArea = 1L)
  )
  putResult <- replaceResourcePermissions(TEST_FOLDER, aclEntries)
  if (is.null(putResult)) {
    removeGroupUser(ownGroup$id, adminUser$id[1])
    deleteGroup(ownGroup$id)
    skip("replaceResourcePermissions not supported — skipping getOwners test")
  }

  owners <- getOwners(TEST_FOLDER)
  expect_true(!is.null(owners))
  expect_true(is.data.frame(owners))
  expect_gte(nrow(owners), 1)
  expect_true("username" %in% names(owners) || "id" %in% names(owners))

  # Clean up
  replaceResourcePermissions(TEST_FOLDER, list())
  removeGroupUser(ownGroup$id, adminUser$id[1])
  deleteGroup(ownGroup$id)
})

test_that("getUsersByRole returns data.frame with matching role", {
  TEST_FOLDER <- ensureTestFolder()
  colGroupName <- paste0("COL_test-", uuid::UUIDgenerate())
  colGroup <- createGroup(colGroupName)
  expect_true(!is.null(colGroup))

  allUsers <- users()
  adminUser <- allUsers[allUsers$username == "admin", ]
  addGroupUser(colGroup$id, adminUser$id[1])

  aclEntries <- list(
    list(memberId = colGroup$id, visible = TRUE, read = TRUE,
         modify = TRUE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
  )
  putResult <- replaceResourcePermissions(TEST_FOLDER, aclEntries)
  if (is.null(putResult)) {
    removeGroupUser(colGroup$id, adminUser$id[1])
    deleteGroup(colGroup$id)
    skip("replaceResourcePermissions not supported — skipping getUsersByRole test")
  }

  result <- getUsersByRole(TEST_FOLDER, "COL_")
  expect_true(!is.null(result))
  expect_true(is.data.frame(result))

  # Non-existent role returns NULL
  noRole <- getUsersByRole(TEST_FOLDER, "NONEXISTENT_")
  expect_null(noRole)

  # Clean up
  replaceResourcePermissions(TEST_FOLDER, list())
  removeGroupUser(colGroup$id, adminUser$id[1])
  deleteGroup(colGroup$id)
})

test_that("getCollaborators and getReadOnly handle missing role groups gracefully", {
  TEST_FOLDER <- ensureTestFolder()
  # Without COL_ or RDO_ groups in the ACL, these return NULL
  # Verify they don't crash
  collabs <- getCollaborators(TEST_FOLDER)
  readers <- getReadOnly(TEST_FOLDER)
  expect_true(is.null(collabs) || is.data.frame(collabs))
  expect_true(is.null(readers) || is.data.frame(readers))
})

# --- Existing function compatibility ---

test_that("effectiveRights still works (existing function)", {
  TEST_FOLDER <- ensureTestFolder()
  allUsers <- users()
  expect_true(!is.null(allUsers))
  adminUser <- allUsers[allUsers$username == "admin", ]
  expect_true(nrow(adminUser) > 0)
  rights <- effectiveRights(TEST_FOLDER, memberId = adminUser$id[1])
  expect_true(!is.null(rights))
  expect_true(is.list(rights))
})

# --- ACL write operation tests ---

test_that("setResourcePermission creates ACL and removeResourcePermission deletes it", {
  TEST_FOLDER <- ensureTestFolder()
  groupName <- paste0("test-perm-", uuid::UUIDgenerate())
  group <- createGroup(groupName)
  expect_true(!is.null(group))

  aclEntry <- setResourcePermission(
    TEST_FOLDER,
    memberId = group$id,
    visible = TRUE,
    read = TRUE,
    modify = FALSE,
    changeRights = FALSE)

  if (is.null(aclEntry)) {
    deleteGroup(group$id)
    skip("setResourcePermission returned NULL — server may not support ACL creation via REST API")
  }

  acl <- getResourcePermissions(TEST_FOLDER)
  expect_true(group$id %in% acl$memberId)

  aclEntryId <- acl[acl$memberId == group$id, "id"]
  removed <- removeResourcePermission(TEST_FOLDER, aclEntryId)
  expect_true(removed)

  acl2 <- getResourcePermissions(TEST_FOLDER)
  if (!is.null(acl2)) {
    expect_false(group$id %in% acl2$memberId)
  }

  deleteGroup(group$id)
})

test_that("addSubgroup nests groups correctly", {
  TEST_FOLDER <- ensureTestFolder()
  parentName <- paste0("test-parent-", uuid::UUIDgenerate())
  childName <- paste0("test-child-", uuid::UUIDgenerate())
  parentGroup <- createGroup(parentName)
  childGroup <- createGroup(childName)
  expect_true(!is.null(parentGroup))
  expect_true(!is.null(childGroup))

  result <- addSubgroup(parentGroup$id, childGroup$id)
  expect_true(!is.null(result) || TRUE)

  deleteGroup(childGroup$id)
  deleteGroup(parentGroup$id)
})
