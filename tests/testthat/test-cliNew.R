Sys.setenv(TEST_NAME = "cliNew")

# ── Helpers ───────────────────────────────────────────────────────────────────

ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME = "cliNew")
  if (!exists("TEST_FOLDER") || is.null(TEST_FOLDER)) {
    improveR::setEditable(TRUE)
    TEST_FOLDER <- improveR:::workflowFilesSetup()
    assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
  }
  return(get("TEST_FOLDER", envir = globalenv()))
}

# Create a step with real file copies (not links), clone to /tmp.
# Returns list(stepRes, localPath, treePath).
setupStepRepo <- function(TEST_FOLDER) {
  r_runserver <- Sys.getenv("R_RUNSERVER")
  r_tool <- Sys.getenv("R_TOOL")
  r_tool_instance <- Sys.getenv("R_TOOL_INSTANCE")

  # No sample() in a test name: a random draw cannot be replayed, and the draw
  # was only guarding against a second name built in the same second - which
  # uniqueTag() already rules out (IMR-297).
  treeName <- paste0("cli_", uniqueTag(6))
  testTree <- createAnalysisTree(targetIdent = TEST_FOLDER, treeName = treeName)

  stepEnv <- createStepTemplateEnv(treeIdent = testTree)
  stepEnv$setStepRunserverLabel(r_runserver)
  stepEnv$setStepToolLabel(r_tool)
  stepEnv$setStepToolInstance(r_tool_instance)
  stepEnv$setStepDescription("CLI test step")
  stepEnv$setStepRationale("automated CLI test")
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/DataManipulation.R"),
                            variableName = "command-file", asLink = FALSE)
  stepEnv$addStepRemoteFile(paste0(TEST_FOLDER, "/data.csv"), asLink = FALSE)

  step <- stepEnv$realise(run = FALSE)
  stepRes <- step$getStepResource()

  localPath <- file.path("/tmp", paste0("cli_", uniqueTag(6)))
  dir.create(localPath, showWarnings = FALSE)
  cloneCli(stepRes$path, localPath)

  # Set input patterns so push knows which files are inputs.
  # add-inputs CLI command has a bug (NPE on FileLock), so write directly.
  writeLines('{"filePatterns":["*.R","*.csv"]}',
             file.path(localPath, ".improve", "input-config.json"))

  list(stepRes = stepRes, localPath = localPath, treePath = testTree$path)
}

# ── Group 1: Detection ───────────────────────────────────────────────────────

test_that("detectCli finds JAR and reports version|cliDetect", {
  improveR:::resetCliDetection()
  improveR:::detectCli()

  mode <- improveR:::cliMode()
  expect_true(mode %in% c("jar", "legacy_binary", "none"))

  if (mode == "jar") {
    version <- improveR:::cliDetectedVersion()
    expect_true(grepl("^\\d+\\.\\d+\\.\\d+$", version),
                info = paste("Version should match X.Y.Z, got:", version))
  }
})

test_that("versionCli returns parseable version string|cliDetect", {
  skip_if(improveR:::cliMode() == "none", "No CLI available")
  result <- versionCli()
  expect_true(any(grepl("\\d+\\.\\d+\\.\\d+", result)),
              info = "Version output should contain X.Y.Z")
})

test_that("IMPROVE_CLI_PATH with nonexistent path falls back to JAR|cliDetect", {
  skip_if(improveR:::cliMode() == "none", "No CLI available")
  origPath <- Sys.getenv("IMPROVE_CLI_PATH", unset = NA)
  on.exit({
    if (is.na(origPath)) Sys.unsetenv("IMPROVE_CLI_PATH") else Sys.setenv(IMPROVE_CLI_PATH = origPath)
    improveR:::resetCliDetection()
  }, add = TRUE)

  Sys.setenv(IMPROVE_CLI_PATH = "/nonexistent/path/improve-cli")
  improveR:::resetCliDetection()
  improveR:::detectCli()
  expect_true(improveR:::cliMode() != "legacy_binary")
})

# ── Group 2: Clone + Status ──────────────────────────────────────────────────

test_that("clone step creates writable workspace|cliClone", {
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  expect_true(dir.exists(file.path(repo$localPath, ".improve")))
  expect_true(file.exists(file.path(repo$localPath, "DataManipulation.R")))
  expect_true(file.exists(file.path(repo$localPath, "data.csv")))

  # Files should be writable (copies, not links)
  expect_true(file.access(file.path(repo$localPath, "DataManipulation.R"), 2) == 0,
              info = "Input file copy should be writable")
})

# ── Group 3: Push ─────────────────────────────────────────────────────────────

test_that("push modified input file, verify via second clone|cliPush", {
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  writeLines("# pushed modification", file.path(repo$localPath, "DataManipulation.R"))
  pushCli(repo$localPath, comment = "push test")

  # Clone to second repo and verify
  repo2 <- file.path("/tmp", paste0("verify_", uniqueTag(6)))
  dir.create(repo2, showWarnings = FALSE)
  on.exit(unlink(repo2, recursive = TRUE), add = TRUE)

  cloneCli(repo$stepRes$path, repo2)
  content <- readLines(file.path(repo2, "DataManipulation.R"))
  expect_equal(content, "# pushed modification",
               info = "Second clone should have the pushed modification")
})

test_that("push --preview does not write to server|cliPush", {
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  originalContent <- readLines(file.path(repo$localPath, "DataManipulation.R"))
  writeLines("# should not appear on server", file.path(repo$localPath, "DataManipulation.R"))

  # preview is computed in improveR, not passed to the CLI: the released CLI
  # has no --preview, and before IMR-283 this call performed the push while
  # reporting a dry run. The returned frame must NAME the change, not merely
  # exist - a preview that reports nothing would pass the server-unchanged
  # assertion below just as well.
  planned <- pushCli(repo$localPath, comment = "preview", preview = TRUE)
  expect_true(is.data.frame(planned))
  expect_true("DataManipulation.R" %in% planned$path,
              info = paste0("the modified file must appear in the preview; got: ",
                            paste(planned$path, collapse = ", ")))
  expect_equal(planned$change[planned$path == "DataManipulation.R"], "modified")

  # Clone fresh to verify server unchanged
  repo2 <- file.path("/tmp", paste0("preview_", uniqueTag(6)))
  dir.create(repo2, showWarnings = FALSE)
  on.exit(unlink(repo2, recursive = TRUE), add = TRUE)

  cloneCli(repo$stepRes$path, repo2)
  content <- readLines(file.path(repo2, "DataManipulation.R"))
  expect_equal(content, originalContent,
               info = "Server content should be unchanged after preview push")
})

# ── Group 4: Pull ─────────────────────────────────────────────────────────────

test_that("pull downloads server changes|cliPull", {
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  # Clone to second repo
  repo2 <- file.path("/tmp", paste0("pull_", uniqueTag(6)))
  dir.create(repo2, showWarnings = FALSE)
  on.exit(unlink(repo2, recursive = TRUE), add = TRUE)
  cloneCli(repo$stepRes$path, repo2)

  # Modify in first repo and push
  writeLines("# from repo1", file.path(repo$localPath, "DataManipulation.R"))
  pushCli(repo$localPath, comment = "for pull test")

  # Pull into second repo
  pullCli(repo2)
  content <- readLines(file.path(repo2, "DataManipulation.R"))
  expect_equal(content, "# from repo1",
               info = "Pull should download the pushed change")
})

test_that("pull --preview does not write locally|cliPull", {
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  repo2 <- file.path("/tmp", paste0("pullprev_", uniqueTag(6)))
  dir.create(repo2, showWarnings = FALSE)
  on.exit(unlink(repo2, recursive = TRUE), add = TRUE)
  cloneCli(repo$stepRes$path, repo2)

  originalContent <- readLines(file.path(repo2, "DataManipulation.R"))

  writeLines("# server change", file.path(repo$localPath, "DataManipulation.R"))
  pushCli(repo$localPath, comment = "for preview pull")

  # As with push, preview is computed here rather than handed to the CLI
  # (IMR-283). The anchor is the child's resourceVersionId against the
  # CheckoutVersionId recorded at clone time - pull.revisionId and
  # entityVersionId were both tried and line up with nothing, so a preview
  # built on either reports changes on an up-to-date clone.
  incoming <- pullCli(repo2, preview = TRUE)
  expect_true(is.data.frame(incoming))
  expect_true("DataManipulation.R" %in% incoming$path,
              info = paste0("the file changed on the server must appear in the ",
                            "preview; got: ", paste(incoming$path, collapse = ", ")))
  expect_equal(incoming$change[incoming$path == "DataManipulation.R"], "modified")

  content <- readLines(file.path(repo2, "DataManipulation.R"))
  expect_equal(content, originalContent,
               info = "Preview pull should not modify local files")
})

# ── Group 5: Conflict ─────────────────────────────────────────────────────────

# ── Group 6: Push Run ─────────────────────────────────────────────────────────

test_that("push run uploads inputs and outputs, creates a run|cliPushRun", {
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  # Modify an input file
  writeLines("# modified input", file.path(repo$localPath, "DataManipulation.R"))

  # Two new files, and only one of them is declared as an input.
  #
  # The released CLI honours .improve/input-config.json exactly - measured on
  # 2026-09-15: with patterns *.R and *.csv a results.txt stays local; add
  # *.txt and it is pushed. The original block wrote results.txt and expected
  # it on the server as an "output" while the fixture declared neither. That
  # expectation came from the withdrawn CLI surface and had never run against the
  # released CLI, because the block skipped.
  #
  # Asserting both directions is worth more than the original: the declared
  # file must arrive AND the undeclared one must not.
  writeLines("declared output", file.path(repo$localPath, "results.csv"))
  writeLines("undeclared", file.path(repo$localPath, "results.txt"))

  # Push run
  pushRunCli(repo$localPath, command = "Rscript")

  # Verify: clone to second repo, both files should be there
  repo2 <- file.path("/tmp", paste0("pushrun_", uniqueTag(6)))
  dir.create(repo2, showWarnings = FALSE)
  on.exit(unlink(repo2, recursive = TRUE), add = TRUE)
  cloneCli(repo$stepRes$path, repo2)

  expect_equal(readLines(file.path(repo2, "DataManipulation.R")), "# modified input",
               info = "Modified input should be on server after push run")
  expect_true(file.exists(file.path(repo2, "results.csv")),
              info = "a file matching the declared input patterns must reach the server")
  expect_equal(readLines(file.path(repo2, "results.csv")), "declared output")
  expect_false(file.exists(file.path(repo2, "results.txt")),
               info = paste0("a file matching NO declared pattern must stay local - ",
                             "input-config.json is the rule, not a suggestion"))

  # Verify a new run was created
  stepRes <- improveR::refreshResource(repo$stepRes$resourceId)
  expect_true(stepRes$runStatus %in% c("RUNNING", "FINISHED", "QUEUED"),
              info = "Step should have a run after push run")
})

# ── Group 7: Tools + Info ────────────────────────────────────────────────────

# ── Group 8: Diff ────────────────────────────────────────────────────────────

# ── Group 9: Error handling ──────────────────────────────────────────────────

test_that("executeCli handles invalid command without crashing|cliError", {
  skip_if(improveR:::cliMode() == "none", "No CLI available")
  # Should not crash R, just return output with non-zero exit
  result <- improveR:::executeCli(c("nonexistent_command_xyz"))
  expect_true(is.character(result))
})

test_that("a preview on an unchanged clone reports nothing|cliPreview", {
  # The counterpart that matters: a preview must be quiet when there is nothing
  # to report. Without this, a preview that always returns an empty frame - or
  # one that always reports everything - passes the blocks above.
  TEST_FOLDER <- ensureTestFolder()
  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  expect_equal(nrow(pushCli(repo$localPath, preview = TRUE)), 0L,
               info = "a clone nobody touched has nothing to push")
  expect_equal(nrow(pullCli(repo$localPath, preview = TRUE)), 0L,
               info = "a clone nobody touched is up to date")
})

test_that("force refuses instead of running without it|cliPush", {
  # There is no local equivalent to compute, so this one is refused rather than
  # emulated. Silently dropping it is what IMR-283 is about.
  TEST_FOLDER <- ensureTestFolder()
  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  expect_error(pushCli(repo$localPath, force = TRUE), "not supported by this CLI")
  expect_error(pushRunCli(repo$localPath, force = TRUE), "not supported by this CLI")
})
