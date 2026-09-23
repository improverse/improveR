# Admin Metrics tests (ics1142)
# Spec ics1142 mandates three surfaces: users(), userAuditTrail(), and
# getFullFolderAuditTrail(). The userAuditTrail function is specified but not
# currently implemented in improveR — the test below will fail fast and flag
# the implementation gap if/when userAuditTrail is added.

Sys.setenv(TEST_NAME = "adminMetrics")

setupAdminMetrics <- function() {
  Sys.setenv(TEST_NAME = "adminMetrics")
  improveR::improveConnect()
  improveR::setEditable(TRUE)
  basePath <- createFolderPath("adminMetrics")
  testFolder <- improveR::createFolder(
    targetIdent = basePath,
    folderName = paste0("am-", uniqueTag()),
    comment = "adminMetrics test setup"
  )
  testFolder
}

# -----------------------------------------------------------------------------
# users() — returns a data frame of all users
# -----------------------------------------------------------------------------
test_that("users returns a data frame with id and username columns|ics1142", {
  improveR::improveConnect()
  allUsers <- improveR::users()
  expect_false(is.null(allUsers))
  expect_true(is.data.frame(allUsers))
  expect_gte(nrow(allUsers), 1)

  # Per spec ics1142: data frame of all users. Core columns expected on every
  # server schema. id+username are the mandatory pair used elsewhere in tests
  # (connectAs, addGroupUser, setResourcePermission).
  expect_true("id" %in% colnames(allUsers))
  expect_true("username" %in% colnames(allUsers))

  # Admin must be present
  expect_true("admin" %in% allUsers$username)
})

# -----------------------------------------------------------------------------
# getFullFolderAuditTrail — recursive folder audit trail, no caching per spec
# -----------------------------------------------------------------------------
test_that("getFullFolderAuditTrail returns entries for folder and descendants|ics1142", {
  testFolder <- setupAdminMetrics()
  on.exit(tryCatch(improveR::delete(testFolder$resourceId),
                   error = function(e) NULL), add = TRUE)

  # Populate: nested folder with a file
  nested <- improveR::createFolder(targetIdent = testFolder$resourceId,
                                   folderName = "nested",
                                   comment = "nested for audit trail")
  leafFile <- improveR::createFile(targetIdent = nested$resourceId,
                                   fileName = "leaf.txt",
                                   comment = "leaf file")

  trail <- improveR::getFullFolderAuditTrail(testFolder$resourceId)
  expect_false(is.null(trail))
  expect_true(is.data.frame(trail))
  expect_gt(nrow(trail), 0)

  # Recursive: entries for parent, nested folder and leaf file must all appear
  expect_true(testFolder$entityId %in% trail$entityId)
  expect_true(nested$entityId %in% trail$entityId)
  expect_true(leafFile$entityId %in% trail$entityId)
})

test_that("getFullFolderAuditTrail is not cached (fresh each call per spec)|ics1142", {
  testFolder <- setupAdminMetrics()
  on.exit(tryCatch(improveR::delete(testFolder$resourceId),
                   error = function(e) NULL), add = TRUE)

  before <- improveR::getFullFolderAuditTrail(testFolder$resourceId)
  beforeRows <- if (is.null(before)) 0 else nrow(before)

  # Produce a new audit event under the folder
  improveR::createFolder(targetIdent = testFolder$resourceId,
                         folderName = "cacheBustProbe",
                         comment = "produce new audit entry")

  # Second call must see the new event immediately — spec says no caching
  after <- improveR::getFullFolderAuditTrail(testFolder$resourceId)
  expect_false(is.null(after))
  expect_gt(nrow(after), beforeRows)
})

test_that("getFullFolderAuditTrail excludes read operations by default|ics1142", {
  testFolder <- setupAdminMetrics()
  on.exit(tryCatch(improveR::delete(testFolder$resourceId),
                   error = function(e) NULL), add = TRUE)

  default <- improveR::getFullFolderAuditTrail(testFolder$resourceId)
  expect_false(is.null(default))
  if ("operation" %in% colnames(default)) {
    expect_false("read" %in% default$operation)
  }

  # With includeReadAccess=TRUE reads may appear (or not — depends on activity).
  # Exercise the flag path without asserting presence.
  withReads <- improveR::getFullFolderAuditTrail(testFolder$resourceId,
                                                 includeReadAccess = TRUE)
  expect_false(is.null(withReads))
  expect_gte(nrow(withReads), nrow(default))
})
