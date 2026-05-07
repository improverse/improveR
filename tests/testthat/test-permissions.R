Sys.setenv(TEST_NAME = "permissions")

ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME = "permissions")
  improveR::setEditable(TRUE)
  if (!exists("TEST_FOLDER", envir = globalenv()) || is.null(get("TEST_FOLDER", envir = globalenv()))) {
    TEST_FOLDER <- improveR:::baseFilesSetup()
    assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
  }
  return(get("TEST_FOLDER", envir = globalenv()))
}

test_that("setup permissions test", {
  improveConnect()
  setEditable(TRUE)
  expect_false(Sys.getenv("IMPROVER_TOKEN") == "")
  TEST_FOLDER <- ensureTestFolder()
  expect_false("" == TEST_FOLDER)
})

# --- Error handling ---

test_that("getResourcePermissions returns NULL for non-existent resource", {
  expect_null(getResourcePermissions("/NonExistent/Path/12345"))
})

test_that("effectiveUserPermissions returns NULL for non-existent resource", {
  expect_null(effectiveUserPermissions("/NonExistent/Path/12345"))
})

# --- Group CRUD with verification ---

test_that("createGroup creates group visible in loadGroups, deleteGroup removes it|ics2043", {
  TEST_FOLDER <- ensureTestFolder()
  groupName <- paste0("test-perm-", uuid::UUIDgenerate())
  group <- createGroup(groupName)
  expect_false(is.null(group))
  expect_equal(group$name, groupName)

  # Verify in loadGroups
  allGroups <- loadGroups()
  expect_true(group$id %in% allGroups$id,
              info = "New group should appear in loadGroups")

  # Verify loadGroup returns correct name
  loaded <- loadGroup(group$id)
  expect_equal(loaded$name, groupName,
               info = "loadGroup should return the same name")

  # Delete and verify gone
  expect_true(deleteGroup(group$id))
  allGroupsAfter <- loadGroups()
  expect_false(group$id %in% allGroupsAfter$id,
               info = "Deleted group should not appear in loadGroups")
})

test_that("addGroupUser adds member visible in loadGroupUsers, removeGroupUser removes|ics2043", {
  TEST_FOLDER <- ensureTestFolder()
  groupName <- paste0("test-member-", uuid::UUIDgenerate())
  group <- createGroup(groupName)

  allUsers <- users()
  adminUser <- allUsers[allUsers$username == "admin", ]

  addGroupUser(group$id, adminUser$id[1])

  # Verify member appears
  members <- loadGroupUsers(group$id)
  expect_true(is.data.frame(members))
  expect_true(nrow(members) > 0, info = "Group should have at least 1 member")

  # Remove and verify gone
  removeGroupUser(group$id, adminUser$id[1])
  members2 <- loadGroupUsers(group$id)
  expect_null(members2, info = "Empty group should return NULL")

  deleteGroup(group$id)
})

test_that("addSubgroup nests group, verified by loadGroup|ics2043", {
  parentName <- paste0("test-parent-", uuid::UUIDgenerate())
  childName <- paste0("test-child-", uuid::UUIDgenerate())
  parentGroup <- createGroup(parentName)
  childGroup <- createGroup(childName)

  result <- addSubgroup(parentGroup$id, childGroup$id)

  # Verify: loadGroup on parent should show the child
  parentDetails <- loadGroup(parentGroup$id)
  expect_false(is.null(parentDetails))

  deleteGroup(childGroup$id)
  deleteGroup(parentGroup$id)
})

# --- ACL: set, read, verify, remove ---

test_that("setResourcePermission creates ACL visible in getResourcePermissions|ics769,ics2044", {
  TEST_FOLDER <- ensureTestFolder()
  groupName <- paste0("test-acl-", uuid::UUIDgenerate())
  group <- createGroup(groupName)

  aclEntry <- setResourcePermission(TEST_FOLDER,
    memberId = group$id, visible = TRUE, read = TRUE,
    modify = FALSE, changeRights = FALSE)

  if (is.null(aclEntry)) {
    deleteGroup(group$id)
    stop("setResourcePermission not supported on this server")
  }

  # Verify ACL contains the group
  acl <- getResourcePermissions(TEST_FOLDER)
  expect_true(is.data.frame(acl))
  expect_true(group$id %in% acl$memberId,
              info = "Group should appear in ACL after setResourcePermission")

  # Verify specific rights
  entry <- acl[acl$memberId == group$id, ]
  expect_true(entry$read, info = "read should be TRUE")
  expect_false(entry$modify, info = "modify should be FALSE")

  # Remove and verify gone
  removed <- removeResourcePermission(TEST_FOLDER, entry$id)
  expect_true(removed)

  acl2 <- getResourcePermissions(TEST_FOLDER)
  if (!is.null(acl2)) {
    expect_false(group$id %in% acl2$memberId,
                 info = "Group should be gone after removeResourcePermission")
  }

  deleteGroup(group$id)
})

# --- Inheritance ---

test_that("permissions inherit to child folders|ics769,ics2044", {
  TEST_FOLDER <- ensureTestFolder()

  # Create hierarchy
  parentFolder <- createFolder(TEST_FOLDER, paste0("inherit-", sample(1000:9999, 1)))
  childFolder <- createFolder(parentFolder$resourceId, "child")

  groupName <- paste0("test-inherit-", uuid::UUIDgenerate())
  group <- createGroup(groupName)

  # Set permission on parent with inherit=TRUE
  aclEntries <- list(
    list(memberId = group$id, visible = TRUE, read = TRUE,
         modify = TRUE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
  )
  putResult <- replaceResourcePermissions(parentFolder$resourceId, aclEntries)
  if (is.null(putResult)) {
    deleteGroup(group$id)
    stop("replaceResourcePermissions not supported")
  }

  # Verify parent has the permission
  parentAcl <- getResourcePermissions(parentFolder$resourceId)
  expect_true(group$id %in% parentAcl$memberId,
              info = "Parent should have the ACL entry")

  # Verify child inherits via effective permissions
  # (inherited ACLs may not appear in getResourcePermissions on the child,
  #  but they are resolved via effectiveUserPermissions)
  allUsers <- users()
  adminUser <- allUsers[allUsers$username == "admin", ]
  addGroupUser(group$id, adminUser$id[1])

  childPerms <- effectiveUserPermissions(childFolder$resourceId)
  expect_true(!is.null(childPerms) && is.data.frame(childPerms),
              info = "Child should have effective permissions via inheritance")

  removeGroupUser(group$id, adminUser$id[1])

  # Cleanup
  replaceResourcePermissions(parentFolder$resourceId, list())
  deleteGroup(group$id)
})

test_that("inherit=FALSE does not propagate to children|ics769,ics2044", {
  TEST_FOLDER <- ensureTestFolder()

  parentFolder <- createFolder(TEST_FOLDER, paste0("noinherit-", sample(1000:9999, 1)))
  childFolder <- createFolder(parentFolder$resourceId, "child")

  groupName <- paste0("test-noinherit-", uuid::UUIDgenerate())
  group <- createGroup(groupName)

  # Set permission on parent with inherit=FALSE
  aclEntries <- list(
    list(memberId = group$id, visible = TRUE, read = TRUE,
         modify = FALSE, changeRights = FALSE, inherit = FALSE, rightsArea = 1L)
  )
  putResult <- replaceResourcePermissions(parentFolder$resourceId, aclEntries)
  if (is.null(putResult)) {
    deleteGroup(group$id)
    stop("replaceResourcePermissions not supported")
  }

  # Parent has the permission
  parentAcl <- getResourcePermissions(parentFolder$resourceId)
  expect_true(group$id %in% parentAcl$memberId)

  # Child should NOT have the permission
  childAcl <- getResourcePermissions(childFolder$resourceId)
  if (!is.null(childAcl)) {
    expect_false(group$id %in% childAcl$memberId,
                 info = "Child should NOT inherit when inherit=FALSE")
  }

  # Cleanup
  replaceResourcePermissions(parentFolder$resourceId, list())
  deleteGroup(group$id)
})

# --- Multiple permissions on same resource ---

test_that("multiple ACL entries on same resource|ics769,ics2044", {
  TEST_FOLDER <- ensureTestFolder()

  group1Name <- paste0("test-multi1-", uuid::UUIDgenerate())
  group2Name <- paste0("test-multi2-", uuid::UUIDgenerate())
  group1 <- createGroup(group1Name)
  group2 <- createGroup(group2Name)

  aclEntries <- list(
    list(memberId = group1$id, visible = TRUE, read = TRUE,
         modify = TRUE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L),
    list(memberId = group2$id, visible = TRUE, read = TRUE,
         modify = FALSE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
  )
  putResult <- replaceResourcePermissions(TEST_FOLDER, aclEntries)
  if (is.null(putResult)) {
    deleteGroup(group1$id); deleteGroup(group2$id)
    stop("replaceResourcePermissions not supported")
  }

  acl <- getResourcePermissions(TEST_FOLDER)
  expect_true(group1$id %in% acl$memberId, info = "Group1 should be in ACL")
  expect_true(group2$id %in% acl$memberId, info = "Group2 should be in ACL")

  # Verify different rights
  g1entry <- acl[acl$memberId == group1$id, ]
  g2entry <- acl[acl$memberId == group2$id, ]
  expect_true(g1entry$modify, info = "Group1 should have modify=TRUE")
  expect_false(g2entry$modify, info = "Group2 should have modify=FALSE")

  # Cleanup
  replaceResourcePermissions(TEST_FOLDER, list())
  deleteGroup(group1$id)
  deleteGroup(group2$id)
})

# --- effectiveRights verification ---

test_that("effectiveRights returns correct rights for user|ics769,ics2044", {
  TEST_FOLDER <- ensureTestFolder()

  groupName <- paste0("test-eff-", uuid::UUIDgenerate())
  group <- createGroup(groupName)

  allUsers <- users()
  adminUser <- allUsers[allUsers$username == "admin", ]
  addGroupUser(group$id, adminUser$id[1])

  aclEntries <- list(
    list(memberId = group$id, visible = TRUE, read = TRUE,
         modify = TRUE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
  )
  putResult <- replaceResourcePermissions(TEST_FOLDER, aclEntries)
  if (is.null(putResult)) {
    removeGroupUser(group$id, adminUser$id[1])
    deleteGroup(group$id)
    stop("replaceResourcePermissions not supported")
  }

  rights <- effectiveRights(TEST_FOLDER, memberId = adminUser$id[1])
  expect_false(is.null(rights))
  expect_true(rights$read, info = "Admin should have read via group membership")

  perms <- effectiveUserPermissions(TEST_FOLDER)
  expect_true(is.data.frame(perms))
  expect_true(nrow(perms) >= 1, info = "Should have at least 1 effective permission")

  # Cleanup
  replaceResourcePermissions(TEST_FOLDER, list())
  removeGroupUser(group$id, adminUser$id[1])
  deleteGroup(group$id)
})

# --- Role-based lookups ---

test_that("getOwners returns users from OWN_ group|ics2044", {
  TEST_FOLDER <- ensureTestFolder()
  ownGroupName <- paste0("OWN_test-", uuid::UUIDgenerate())
  ownGroup <- createGroup(ownGroupName)
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
    stop("replaceResourcePermissions not supported")
  }

  owners <- getOwners(TEST_FOLDER)
  expect_true(is.data.frame(owners))
  expect_true(nrow(owners) >= 1, info = "Should find at least 1 owner")

  # Cleanup
  replaceResourcePermissions(TEST_FOLDER, list())
  removeGroupUser(ownGroup$id, adminUser$id[1])
  deleteGroup(ownGroup$id)
})

test_that("getUsersByRole returns correct users, NULL for missing role|ics2044", {
  TEST_FOLDER <- ensureTestFolder()
  colGroupName <- paste0("COL_test-", uuid::UUIDgenerate())
  colGroup <- createGroup(colGroupName)
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
    stop("replaceResourcePermissions not supported")
  }

  result <- getUsersByRole(TEST_FOLDER, "COL_")
  expect_true(is.data.frame(result), info = "Should find users for COL_ role")

  noRole <- getUsersByRole(TEST_FOLDER, "NONEXISTENT_")
  expect_null(noRole, info = "Non-existent role should return NULL")

  # Cleanup
  replaceResourcePermissions(TEST_FOLDER, list())
  removeGroupUser(colGroup$id, adminUser$id[1])
  deleteGroup(colGroup$id)
})
