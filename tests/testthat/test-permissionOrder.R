# What the server actually does with orderNr (IMR-305)
#
# improveR documents "Lower numbers are evaluated first. Must be unique per
# resource." ics2044 says "evaluation priority, lower is evaluated first".
# ics769, which DEFINES the field, says only "position in the ACL list".
# Nothing states what happens when two entries conflict, and until now nothing
# tested it - the one existing test asserts that orderNr comes back as 1 and 2,
# without checking which member got which, and without asserting any effect.
#
# Measured against repository 4315 on 2026-09-18, and the assertions below are
# what was measured:
#
#   modify = TRUE  at orderNr 1  ->  test1 effectively HAS modify
#   modify = FALSE at orderNr 1  ->  test1 effectively has NOT
#   a second entry with a duplicate orderNr  ->  REFUSED by the server
#
# So the rule is FIRST MATCH WINS, not most-permissive-wins, and uniqueness is
# enforced by the server rather than merely asserted by this client. improveR's
# own documentation was right on both counts; ics769 understates it (IMR-305).
Sys.setenv(TEST_NAME = "permissionOrder")

PO <- new.env(parent = emptyenv())

hasConnectAs <- function() {
  "improveRtestsupport" %in% loadedNamespaces() &&
    exists("connectAs", envir = asNamespace("improveRtestsupport"))
}

test_that("setup permissionOrder|IMR-305", {
  skip_if(!hasConnectAs(), "multi-user testbed required")
  improveR::improveConnect()
  improveR::setEditable(TRUE)

  base <- createFolderPath("permissionOrder")
  PO$FOLDER <- improveR::createFolder(
    targetIdent = base,
    folderName  = paste0("ord-", uniqueTag()),
    comment     = "orderNr precedence probe")
  expect_false(is.null(PO$FOLDER))

  PO$G_YES <- improveR::createGroup(paste0("ORD-yes-", uniqueTag()))
  PO$G_NO  <- improveR::createGroup(paste0("ORD-no-",  uniqueTag()))
  expect_false(is.null(PO$G_YES))
  expect_false(is.null(PO$G_NO))

  users <- improveR::users()
  t1 <- users[users$username == "test1", ]
  skip_if(nrow(t1) == 0, "test1 does not exist on this instance")
  PO$T1 <- t1$id[1]

  # the same user in BOTH groups: that is what makes the entries conflict
  improveR::addGroupUser(PO$G_YES$id, PO$T1)
  improveR::addGroupUser(PO$G_NO$id,  PO$T1)
  assign("PO", PO, envir = globalenv())
  cat("ORDER-PROBE folder:", PO$FOLDER$path, "\n")
})

aclPair <- function(yesFirst) {
  yes <- list(memberId = PO$G_YES$id, visible = TRUE, read = TRUE,
              modify = TRUE,  changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
  no  <- list(memberId = PO$G_NO$id,  visible = TRUE, read = TRUE,
              modify = FALSE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
  if (yesFirst) list(yes, no) else list(no, yes)
}

# effectiveUserPermissions() is documented as "Get Effective Permissions for All
# Users" and that is exactly what it returns - one row per user on the instance,
# with user.username to identify them. An earlier version of this probe read
# row 1 and reported whatever the first user happened to have; on this instance
# that is a user with modify regardless, so it "measured" a result that was an
# artefact of the bug. Filter by the user, and no identity switch is needed at
# all: the answer for test1 can be read while connected as the run user.
modifyForTest1 <- function() {
  eff <- tryCatch(improveR::effectiveUserPermissions(PO$FOLDER$path),
                  error = function(e) NULL)
  if (is.null(eff) || !"user.username" %in% names(eff)) return(NA)
  row <- eff[eff$user.username == "test1", , drop = FALSE]
  cat("ORDER-PROBE rows for test1:", nrow(row), "of", nrow(eff), "\n")
  if (nrow(row) != 1L) return(NA)
  row$modify[1]
}

test_that("the ACL is stored in the order it was given|ics2044,IMR-305", {
  PO <- get("PO", envir = globalenv())
  skip_if(is.null(PO$FOLDER), "setup did not run")

  improveR::replaceResourcePermissions(PO$FOLDER$path, aclPair(TRUE))
  acl <- improveR::getResourcePermissions(PO$FOLDER$path)
  expect_equal(nrow(acl), 2)

  # The assertion the existing test is missing: WHICH member got WHICH number.
  # Asserting only that the sequence is 1,2 would hold even if the server had
  # swapped the two entries.
  byMember <- stats::setNames(acl$orderNr, acl$memberId)
  expect_equal(unname(byMember[PO$G_YES$id]), 1L)
  expect_equal(unname(byMember[PO$G_NO$id]),  2L)
})

test_that("the entry with the lower orderNr decides|ics2044,IMR-305", {
  PO <- get("PO", envir = globalenv())
  skip_if(is.null(PO$FOLDER), "setup did not run")
  skip_if(is.null(PO$T1), "test1 not available")

  improveR::replaceResourcePermissions(PO$FOLDER$path, aclPair(TRUE))
  yesFirst <- modifyForTest1()
  cat("ORDER-PROBE modify=TRUE at orderNr 1 -> effective modify:", yesFirst, "\n")

  improveR::replaceResourcePermissions(PO$FOLDER$path, aclPair(FALSE))
  noFirst <- modifyForTest1()
  cat("ORDER-PROBE modify=FALSE at orderNr 1 -> effective modify:", noFirst, "\n")

  # The whole substance of an ordering rule: reversing the order must reverse
  # the outcome. If both came back the same, ordering would decide nothing and
  # the documented "lower numbers are evaluated first" would be decoration.
  expect_true(yesFirst,
              info = "modify=TRUE at orderNr 1 should grant modify")
  expect_false(noFirst,
               info = "modify=FALSE at orderNr 1 should withhold modify - first match wins")
})

test_that("the server refuses a duplicate orderNr|ics2044,IMR-305", {
  PO <- get("PO", envir = globalenv())
  skip_if(is.null(PO$FOLDER), "setup did not run")

  improveR::replaceResourcePermissions(PO$FOLDER$path, list())
  a <- improveR::setResourcePermission(PO$FOLDER$path, PO$G_YES$id, modify = TRUE,  orderNr = 1L)
  b <- improveR::setResourcePermission(PO$FOLDER$path, PO$G_NO$id,  modify = FALSE, orderNr = 1L)

  cat("ORDER-PROBE duplicate orderNr, second call:",
      if (is.null(b)) "REFUSED (NULL)" else "ACCEPTED", "\n")
  acl <- improveR::getResourcePermissions(PO$FOLDER$path)
  cat("ORDER-PROBE resulting rows:", nrow(acl),
      "orderNrs:", paste(acl$orderNr, collapse = ","), "\n")

  expect_false(is.null(a), info = "the first entry should be created")
  # "Must be unique per resource" is improveR's claim about the server. It holds:
  # the server refuses the second entry, so authenticatedREST returns NULL.
  expect_null(b, info = "a duplicate orderNr must be refused, not silently accepted")
  expect_equal(nrow(acl), 1L, info = "the refused entry must not have been stored")
})

test_that("teardown permissionOrder|IMR-305", {
  PO <- get("PO", envir = globalenv())
  skip_if(is.null(PO$FOLDER), "setup did not run")
  improveR::replaceResourcePermissions(PO$FOLDER$path, list())
  improveR::deleteGroup(PO$G_YES$id)
  improveR::deleteGroup(PO$G_NO$id)
  expect_true(TRUE)
})
