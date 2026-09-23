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

  requireServerCall(aclEntry, "setResourcePermission", cleanup = function() { deleteGroup(group$id) })

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
  parentFolder <- createFolder(TEST_FOLDER, paste0("inherit-", uniqueTag(6)))
  childFolder <- createFolder(parentFolder$resourceId, "child")

  groupName <- paste0("test-inherit-", uuid::UUIDgenerate())
  group <- createGroup(groupName)

  # Set permission on parent with inherit=TRUE
  aclEntries <- list(
    list(memberId = group$id, visible = TRUE, read = TRUE,
         modify = TRUE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
  )
  putResult <- replaceResourcePermissions(parentFolder$resourceId, aclEntries)
  requireServerCall(putResult, "replaceResourcePermissions", cleanup = function() { deleteGroup(group$id) })

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

  parentFolder <- createFolder(TEST_FOLDER, paste0("noinherit-", uniqueTag(6)))
  childFolder <- createFolder(parentFolder$resourceId, "child")

  groupName <- paste0("test-noinherit-", uuid::UUIDgenerate())
  group <- createGroup(groupName)

  # Set permission on parent with inherit=FALSE
  aclEntries <- list(
    list(memberId = group$id, visible = TRUE, read = TRUE,
         modify = FALSE, changeRights = FALSE, inherit = FALSE, rightsArea = 1L)
  )
  putResult <- replaceResourcePermissions(parentFolder$resourceId, aclEntries)
  requireServerCall(putResult, "replaceResourcePermissions", cleanup = function() { deleteGroup(group$id) })

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
  requireServerCall(putResult, "replaceResourcePermissions", cleanup = function() { deleteGroup(group1$id); deleteGroup(group2$id) })

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
  requireServerCall(putResult, "replaceResourcePermissions", cleanup = function() { removeGroupUser(group$id, adminUser$id[1]); deleteGroup(group$id) })

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
# --- Rights, asserted by result rather than by shape (IMR-276) -------------
#
# The blocks below replace the two role-prefix tests that were removed with
# getUsersByRole, getOwners, getCollaborators and getReadOnly. Those resolved a
# member's role from a group NAME beginning with "OWN_", "COL_" or "RDO_" - a
# convention no repository carries any more. The tests created the prefixed
# group themselves and then found it again, so what they proved was
# startsWith(), not a rights concept.
#
# What replaces them asserts the right that was granted, on the member it was
# granted to. The previous style,
#
#     expect_true(is.data.frame(perms))
#     expect_true(nrow(perms) >= 1)
#
# passes whether the rights are correct or not.

# One helper, so every block below grants against a known member.
adminMemberId <- function() {
  allUsers <- improveR::users()
  expect_false(is.null(allUsers), info = "users() must return the user list")
  admin <- allUsers[allUsers$username == "admin", ]
  expect_true(nrow(admin) >= 1, info = "the run identity 'admin' must exist")
  admin$id[1]
}

aclFor <- function(perms, memberId) {
  if (is.null(perms) || nrow(perms) == 0) return(NULL)
  hit <- perms[perms$memberId == memberId, ]
  if (nrow(hit) == 0) NULL else hit[1, ]
}

test_that("setResourcePermission grants exactly the rights asked for|ics2044", {
  TEST_FOLDER <- ensureTestFolder()
  member <- adminMemberId()

  created <- improveR::setResourcePermission(
    TEST_FOLDER, memberId = member,
    visible = TRUE, read = TRUE, modify = FALSE, changeRights = FALSE)
  requireServerCall(created, "setResourcePermission")

  entry <- aclFor(improveR::getResourcePermissions(TEST_FOLDER), member)
  expect_false(is.null(entry), info = "the granted member must appear in the ACL")
  expect_true(entry$visible,       info = "visible was granted")
  expect_true(entry$read,          info = "read was granted")
  expect_false(entry$modify,       info = "modify was NOT granted and must be FALSE")
  expect_false(entry$changeRights, info = "changeRights was NOT granted and must be FALSE")

  improveR::replaceResourcePermissions(TEST_FOLDER, list())
})

test_that("updateResourcePermission changes the flag it is given and leaves the rest|ics2044", {
  TEST_FOLDER <- ensureTestFolder()
  member <- adminMemberId()

  created <- improveR::setResourcePermission(
    TEST_FOLDER, memberId = member,
    visible = TRUE, read = TRUE, modify = FALSE, changeRights = FALSE)
  requireServerCall(created, "setResourcePermission")
  before <- aclFor(improveR::getResourcePermissions(TEST_FOLDER), member)
  expect_false(is.null(before))

  updated <- improveR::updateResourcePermission(
    TEST_FOLDER, aclId = before$id, memberId = member,
    visible = TRUE, read = TRUE, modify = TRUE, changeRights = FALSE)
  requireServerCall(updated, "updateResourcePermission",
                    cleanup = function() improveR::replaceResourcePermissions(TEST_FOLDER, list()))

  after <- aclFor(improveR::getResourcePermissions(TEST_FOLDER), member)
  expect_false(is.null(after), info = "the entry must still be there after an update")
  expect_true(after$modify,        info = "modify was the flag changed, it must now be TRUE")
  expect_true(after$read,          info = "read was not touched and must still be TRUE")
  expect_false(after$changeRights, info = "changeRights was not touched and must still be FALSE")

  improveR::replaceResourcePermissions(TEST_FOLDER, list())
})

test_that("removeResourcePermission removes that entry and only that entry|ics2044", {
  TEST_FOLDER <- ensureTestFolder()
  member <- adminMemberId()
  groupName <- paste0("imr276-", uuid::UUIDgenerate())
  group <- improveR::createGroup(groupName)
  requireServerCall(group, "createGroup")

  entries <- list(
    list(memberId = member,   visible = TRUE, read = TRUE, modify = FALSE,
         changeRights = FALSE, inherit = TRUE, rightsArea = 1L),
    list(memberId = group$id, visible = TRUE, read = TRUE, modify = TRUE,
         changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
  )
  requireServerCall(improveR::replaceResourcePermissions(TEST_FOLDER, entries),
                    "replaceResourcePermissions",
                    cleanup = function() improveR::deleteGroup(group$id))

  both <- improveR::getResourcePermissions(TEST_FOLDER)
  expect_equal(nrow(both), 2L, info = "both entries must be present before the removal")
  victim <- aclFor(both, group$id)
  expect_false(is.null(victim))

  expect_true(improveR::removeResourcePermission(TEST_FOLDER, victim$id))

  left <- improveR::getResourcePermissions(TEST_FOLDER)
  expect_equal(nrow(left), 1L, info = "exactly one entry must remain")
  expect_true(is.null(aclFor(left, group$id)), info = "the removed member must be gone")
  expect_false(is.null(aclFor(left, member)),  info = "the other member must be untouched")

  improveR::replaceResourcePermissions(TEST_FOLDER, list())
  improveR::deleteGroup(group$id)
})

test_that("replaceResourcePermissions leaves exactly the set it was given|ics2076", {
  TEST_FOLDER <- ensureTestFolder()
  member <- adminMemberId()
  groupName <- paste0("imr276-", uuid::UUIDgenerate())
  group <- improveR::createGroup(groupName)
  requireServerCall(group, "createGroup")

  requireServerCall(
    improveR::replaceResourcePermissions(TEST_FOLDER, list(
      list(memberId = member, visible = TRUE, read = TRUE, modify = TRUE,
           changeRights = TRUE, inherit = TRUE, rightsArea = 1L))),
    "replaceResourcePermissions",
    cleanup = function() improveR::deleteGroup(group$id))
  first <- improveR::getResourcePermissions(TEST_FOLDER)
  expect_equal(nrow(first), 1L)
  expect_equal(first$memberId[1], member)

  # Replacing is not adding: the previous entry must be gone afterwards.
  requireServerCall(
    improveR::replaceResourcePermissions(TEST_FOLDER, list(
      list(memberId = group$id, visible = TRUE, read = TRUE, modify = FALSE,
           changeRights = FALSE, inherit = TRUE, rightsArea = 1L))),
    "replaceResourcePermissions",
    cleanup = function() improveR::deleteGroup(group$id))
  second <- improveR::getResourcePermissions(TEST_FOLDER)
  expect_equal(nrow(second), 1L, info = "replace must not accumulate entries")
  expect_equal(second$memberId[1], group$id, info = "the new member must be the only one")
  expect_true(is.null(aclFor(second, member)), info = "the previous member must be gone")

  # And the empty list clears it.
  improveR::replaceResourcePermissions(TEST_FOLDER, list())
  cleared <- improveR::getResourcePermissions(TEST_FOLDER)
  expect_true(is.null(cleared) || nrow(cleared) == 0,
              info = "an empty list must clear the ACL")

  improveR::deleteGroup(group$id)
})

test_that("effectiveRights reports the rights that were actually granted|ics2024", {
  TEST_FOLDER <- ensureTestFolder()
  member <- adminMemberId()

  requireServerCall(
    improveR::replaceResourcePermissions(TEST_FOLDER, list(
      list(memberId = member, visible = TRUE, read = TRUE, modify = FALSE,
           changeRights = FALSE, inherit = TRUE, rightsArea = 1L))),
    "replaceResourcePermissions")
  readOnly <- improveR::effectiveRights(TEST_FOLDER, memberId = member)
  requireServerCall(readOnly, "effectiveRights")
  expect_true(readOnly$read,   info = "read was granted")
  expect_false(readOnly$modify, info = "modify was not granted")

  requireServerCall(
    improveR::replaceResourcePermissions(TEST_FOLDER, list(
      list(memberId = member, visible = TRUE, read = TRUE, modify = TRUE,
           changeRights = FALSE, inherit = TRUE, rightsArea = 1L))),
    "replaceResourcePermissions")
  writable <- improveR::effectiveRights(TEST_FOLDER, memberId = member)
  requireServerCall(writable, "effectiveRights")
  expect_true(writable$modify, info = "modify was granted the second time and must follow")

  improveR::replaceResourcePermissions(TEST_FOLDER, list())
})

test_that("effectiveUserPermissions lists the member that was granted, with its rights|ics2044", {
  TEST_FOLDER <- ensureTestFolder()
  member <- adminMemberId()

  requireServerCall(
    improveR::replaceResourcePermissions(TEST_FOLDER, list(
      list(memberId = member, visible = TRUE, read = TRUE, modify = TRUE,
           changeRights = FALSE, inherit = TRUE, rightsArea = 1L))),
    "replaceResourcePermissions")

  perms <- improveR::effectiveUserPermissions(TEST_FOLDER)
  requireServerCall(perms, "effectiveUserPermissions")
  expect_true(is.data.frame(perms))

  # effectiveUserRights resolves groups down to USERS, so the result is keyed by
  # user and not by the ACL's memberId - the columns arrive flattened as
  # user.username, user.name, user.role, user.active. Asserting on memberId
  # finds nothing, which is what the first version of this block did.
  expect_true("user.username" %in% names(perms),
              info = "effectiveUserPermissions must report the user it resolved to")
  expect_true("admin" %in% perms$user.username,
              info = paste0("the identity the right was granted to must appear in the ",
                            "effective permissions; got: ",
                            paste(utils::head(perms$user.username, 5), collapse = ", ")))

  improveR::replaceResourcePermissions(TEST_FOLDER, list())
})

test_that("effectiveUserPermissions returns NULL for a resource that does not exist|ics2044", {
  # Path 1 of three. Path 3 - the success case - is the block above.
  #
  # Path 2, the REST call returning NULL while the resource loads fine, cannot
  # be provoked against a healthy server without injecting a fault, and is
  # deliberately NOT faked here. It is covered by the contract of
  # authenticatedREST()/restContent() (IMR-270), not from this file.
  expect_null(improveR::effectiveUserPermissions("/NonExistent/Path/imr276"))
})

test_that("loadGroups lists a group that was just created, and drops it after deletion|ics2043", {
  groupName <- paste0("imr276-", uuid::UUIDgenerate())
  group <- improveR::createGroup(groupName)
  requireServerCall(group, "createGroup")

  listed <- improveR::loadGroups()
  requireServerCall(listed, "loadGroups",
                    cleanup = function() improveR::deleteGroup(group$id))
  expect_true(groupName %in% listed$name,
              info = "a group that was just created must appear in loadGroups()")

  expect_true(improveR::deleteGroup(group$id))
  after <- improveR::loadGroups()
  expect_false(groupName %in% after$name,
               info = "a deleted group must no longer appear in loadGroups()")
})
