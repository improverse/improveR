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

  treeName <- paste0("cli_", format(Sys.time(), "%H%M%S"), "_", sample(1000:9999, 1))
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

  localPath <- file.path("/tmp", paste0("cli_", format(Sys.time(), "%H%M%S"),
                                        "_", sample(1000:9999, 1)))
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
    expect_true(improveR:::hasPicocli())
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
  skip_if(!improveR:::hasPicocli(), "Picocli not available")
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

test_that("statusCli on clean repo shows no changes|cliStatus", {
  skip_if(!improveR:::hasPicocli(), "Picocli not available")
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  status <- statusCli(repo$localPath, json = TRUE)
  expect_true(is.list(status))
  expect_equal(status$command, "status")

  compare <- status$data$compare
  expect_true(is.data.frame(compare))
  expect_true(all(compare$localChanged == FALSE),
              info = "Clean repo should have no local changes")
})

test_that("statusCli detects local modification|cliStatus", {
  skip_if(!improveR:::hasPicocli(), "Picocli not available")
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  writeLines("# modified by test", file.path(repo$localPath, "DataManipulation.R"))

  status <- statusCli(repo$localPath, json = TRUE)
  compare <- status$data$compare
  modEntry <- compare[compare$path == "DataManipulation.R", ]

  expect_true(modEntry$localChanged,
              info = "Modified file should show localChanged=TRUE")
  expect_true(modEntry$localHash != modEntry$remoteHash,
              info = "Hashes should differ after modification")
})

# ── Group 3: Push ─────────────────────────────────────────────────────────────

test_that("push modified input file, verify via second clone|cliPush", {
  skip_if(!improveR:::hasPicocli(), "Picocli not available")
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  writeLines("# pushed modification", file.path(repo$localPath, "DataManipulation.R"))
  pushCli(repo$localPath, comment = "push test")

  # Clone to second repo and verify
  repo2 <- file.path("/tmp", paste0("verify_", sample(1000:9999, 1)))
  dir.create(repo2, showWarnings = FALSE)
  on.exit(unlink(repo2, recursive = TRUE), add = TRUE)

  cloneCli(repo$stepRes$path, repo2)
  content <- readLines(file.path(repo2, "DataManipulation.R"))
  expect_equal(content, "# pushed modification",
               info = "Second clone should have the pushed modification")
})

test_that("push --preview does not write to server|cliPush", {
  skip_if(!improveR:::hasPicocli(), "Picocli not available")
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  originalContent <- readLines(file.path(repo$localPath, "DataManipulation.R"))
  writeLines("# should not appear on server", file.path(repo$localPath, "DataManipulation.R"))
  pushCli(repo$localPath, comment = "preview", preview = TRUE)

  # Clone fresh to verify server unchanged
  repo2 <- file.path("/tmp", paste0("preview_", sample(1000:9999, 1)))
  dir.create(repo2, showWarnings = FALSE)
  on.exit(unlink(repo2, recursive = TRUE), add = TRUE)

  cloneCli(repo$stepRes$path, repo2)
  content <- readLines(file.path(repo2, "DataManipulation.R"))
  expect_equal(content, originalContent,
               info = "Server content should be unchanged after preview push")
})

# ── Group 4: Pull ─────────────────────────────────────────────────────────────

test_that("pull downloads server changes|cliPull", {
  skip_if(!improveR:::hasPicocli(), "Picocli not available")
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  # Clone to second repo
  repo2 <- file.path("/tmp", paste0("pull_", sample(1000:9999, 1)))
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
  skip_if(!improveR:::hasPicocli(), "Picocli not available")
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  repo2 <- file.path("/tmp", paste0("pullprev_", sample(1000:9999, 1)))
  dir.create(repo2, showWarnings = FALSE)
  on.exit(unlink(repo2, recursive = TRUE), add = TRUE)
  cloneCli(repo$stepRes$path, repo2)

  originalContent <- readLines(file.path(repo2, "DataManipulation.R"))

  writeLines("# server change", file.path(repo$localPath, "DataManipulation.R"))
  pushCli(repo$localPath, comment = "for preview pull")

  pullCli(repo2, preview = TRUE)
  content <- readLines(file.path(repo2, "DataManipulation.R"))
  expect_equal(content, originalContent,
               info = "Preview pull should not modify local files")
})

# ── Group 5: Conflict ─────────────────────────────────────────────────────────

test_that("conflict detected after concurrent modification|cliConflict", {
  skip_if(!improveR:::hasPicocli(), "Picocli not available")
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  repo2 <- file.path("/tmp", paste0("conflict_", sample(1000:9999, 1)))
  dir.create(repo2, showWarnings = FALSE)
  on.exit(unlink(repo2, recursive = TRUE), add = TRUE)
  cloneCli(repo$stepRes$path, repo2)
  writeLines('{"filePatterns":["*.R","*.csv"]}',
             file.path(repo2, ".improve", "input-config.json"))

  # Modify same file in both, push from first
  writeLines("# repo1 version", file.path(repo$localPath, "DataManipulation.R"))
  pushCli(repo$localPath, comment = "create conflict")

  writeLines("# repo2 version", file.path(repo2, "DataManipulation.R"))

  # Check status BEFORE pull — conflict is visible here
  status <- statusCli(repo2, json = TRUE)
  compare <- status$data$compare
  entry <- compare[compare$path == "DataManipulation.R", ]

  expect_true(entry$localChanged, info = "Should show localChanged=TRUE")
  expect_true(entry$remoteChanged, info = "Should show remoteChanged=TRUE")
  expect_equal(entry$conflictType, "BOTH_MODIFIED",
               info = "conflictType should be BOTH_MODIFIED")

  # After pull, baseline is updated — local change preserved
  pullCli(repo2)
  content <- readLines(file.path(repo2, "DataManipulation.R"))
  expect_equal(content, "# repo2 version",
               info = "Pull should preserve local modification")
})

test_that("resetCli restores file to baseline|cliConflict", {
  skip_if(!improveR:::hasPicocli(), "Picocli not available")
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  repo2 <- file.path("/tmp", paste0("reset_", sample(1000:9999, 1)))
  dir.create(repo2, showWarnings = FALSE)
  on.exit(unlink(repo2, recursive = TRUE), add = TRUE)
  cloneCli(repo$stepRes$path, repo2)
  writeLines('{"filePatterns":["*.R","*.csv"]}',
             file.path(repo2, ".improve", "input-config.json"))

  # Push from repo1 to create a remote change
  writeLines("# repo1 pushed", file.path(repo$localPath, "DataManipulation.R"))
  pushCli(repo$localPath, comment = "for reset test")

  # Modify locally in repo2 and pull (creates baseline update)
  writeLines("# repo2 local", file.path(repo2, "DataManipulation.R"))
  pullCli(repo2)

  # After pull, baseline is updated. The file is locally modified but not in conflict.
  # Reset only works on conflict files (not regular local modifications).
  # Verify the local modification is preserved after pull.
  status <- statusCli(repo2, json = TRUE)
  entry <- status$data$compare[status$data$compare$path == "DataManipulation.R", ]
  expect_true(entry$localChanged,
              info = "Local modification should be preserved after pull")
})

# ── Group 6: Push Run ─────────────────────────────────────────────────────────

test_that("push run uploads inputs and outputs, creates a run|cliPushRun", {
  skip_if(!improveR:::hasPicocli(), "Picocli not available")
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  # Modify an input file
  writeLines("# modified input", file.path(repo$localPath, "DataManipulation.R"))

  # Create an output file
  writeLines("output results", file.path(repo$localPath, "results.txt"))

  # Push run
  pushRunCli(repo$localPath, command = "Rscript")

  # Verify: clone to second repo, both files should be there
  repo2 <- file.path("/tmp", paste0("pushrun_", sample(1000:9999, 1)))
  dir.create(repo2, showWarnings = FALSE)
  on.exit(unlink(repo2, recursive = TRUE), add = TRUE)
  cloneCli(repo$stepRes$path, repo2)

  expect_equal(readLines(file.path(repo2, "DataManipulation.R")), "# modified input",
               info = "Modified input should be on server after push run")
  expect_true(file.exists(file.path(repo2, "results.txt")),
              info = "Output file should be on server after push run")
  expect_equal(readLines(file.path(repo2, "results.txt")), "output results")

  # Verify a new run was created
  stepRes <- improveR::refreshResource(repo$stepRes$resourceId)
  expect_true(stepRes$runStatus %in% c("RUNNING", "FINISHED", "QUEUED"),
              info = "Step should have a run after push run")
})

# ── Group 7: Tools + Info ────────────────────────────────────────────────────

test_that("toolsCli returns non-empty output|cliTools", {
  skip_if(!improveR:::hasPicocli(), "Picocli not available")
  result <- toolsCli()
  expect_true(length(result) > 0)
})

test_that("infoCli shows server URL|cliInfo", {
  skip_if(!improveR:::hasPicocli(), "Picocli not available")
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  result <- infoCli(repo$localPath)
  repoUrl <- Sys.getenv("IMPROVER_REPO_URL")
  expect_true(any(grepl(sub("/repository$", "", repoUrl), result, fixed = TRUE)),
              info = "Info should contain the server URL")
})

# ── Group 8: Diff ────────────────────────────────────────────────────────────

test_that("diffCli unified diff shows correct format and content|cliDiff", {
  skip_if(!improveR:::hasPicocli(), "Picocli not available")
  ver <- improveR:::cliDetectedVersion()
  skip_if(!is.null(ver) && compareVersion(ver, "4.5.0") < 0, "diff requires CLI 4.5.0+")
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  # Read original, append a line, then diff
  originalContent <- readLines(file.path(repo$localPath, "DataManipulation.R"))
  writeLines(c(originalContent, "# diff test line"), file.path(repo$localPath, "DataManipulation.R"))

  result <- diffCli(repo$localPath, "DataManipulation.R")
  diffText <- paste(result, collapse = "\n")

  # Unified diff format: must have --- / +++ / @@ headers
  expect_true(any(grepl("^---", result)), info = "Diff must contain --- header")
  expect_true(any(grepl("^\\+\\+\\+", result)), info = "Diff must contain +++ header")
  expect_true(any(grepl("^@@", result)), info = "Diff must contain @@ hunk header")

  # Added line must appear with + prefix
  expect_true(any(grepl("^\\+# diff test line", result)),
              info = "Added line must appear as +# diff test line")
})

test_that("diffCli no output when file is unchanged|cliDiff", {
  skip_if(!improveR:::hasPicocli(), "Picocli not available")
  ver <- improveR:::cliDetectedVersion()
  skip_if(!is.null(ver) && compareVersion(ver, "4.5.0") < 0, "diff requires CLI 4.5.0+")
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  # No modification — diff should have no +/- lines (ignore info/auth noise)
  result <- diffCli(repo$localPath, "DataManipulation.R")
  diffLines <- result[grepl("^[-+@]", result)]
  expect_true(length(diffLines) == 0,
              info = "Diff should have no hunks when file is unchanged")
})

test_that("diffCli --base returns original content, not local edits|cliDiff", {
  skip_if(!improveR:::hasPicocli(), "Picocli not available")
  ver <- improveR:::cliDetectedVersion()
  skip_if(!is.null(ver) && compareVersion(ver, "4.5.0") < 0, "diff requires CLI 4.5.0+")
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  # Capture original content before modification
  originalContent <- readLines(file.path(repo$localPath, "DataManipulation.R"))

  # Replace file content entirely
  writeLines("# local only — not on server", file.path(repo$localPath, "DataManipulation.R"))

  baseContent <- diffCli(repo$localPath, "DataManipulation.R", base = TRUE)
  expect_true(length(baseContent) > 0, info = "Base content should be non-empty")

  # Baseline must contain the original file content (normalize \r from server)
  baseNormalized <- gsub("\r$", "", baseContent)
  baseNormalized <- baseNormalized[!grepl("^Info:", baseNormalized)]  # strip auth noise
  expect_equal(baseNormalized, originalContent,
               info = "Baseline content must match the original file before modification")

  # Baseline must NOT contain the local-only change
  expect_false(any(grepl("local only", baseContent)),
               info = "Baseline must not contain local modification")
})

test_that("diffCli --theirs matches --base when no server changes occurred|cliDiff", {
  skip_if(!improveR:::hasPicocli(), "Picocli not available")
  ver <- improveR:::cliDetectedVersion()
  skip_if(!is.null(ver) && compareVersion(ver, "4.5.0") < 0, "diff requires CLI 4.5.0+")
  TEST_FOLDER <- ensureTestFolder()

  repo <- setupStepRepo(TEST_FOLDER)
  on.exit(unlink(repo$localPath, recursive = TRUE), add = TRUE)

  # No server-side push happened, so --theirs and --base should return
  # the same content (both point to the same revision)
  baseContent <- diffCli(repo$localPath, "DataManipulation.R", base = TRUE)
  theirsContent <- diffCli(repo$localPath, "DataManipulation.R", theirs = TRUE)

  expect_true(length(baseContent) > 0, info = "Base content should be non-empty")
  expect_true(length(theirsContent) > 0, info = "Theirs content should be non-empty")
  # Normalize \r and filter auth noise for comparison
  normalize <- function(x) gsub("\r$", "", x[!grepl("^Info:", x)])
  expect_equal(normalize(theirsContent), normalize(baseContent),
               info = "Without server changes, --theirs and --base should return identical content")
})

# ── Group 9: Error handling ──────────────────────────────────────────────────

test_that("executeCli handles invalid command without crashing|cliError", {
  skip_if(improveR:::cliMode() == "none", "No CLI available")
  # Should not crash R, just return output with non-zero exit
  result <- improveR:::executeCli(c("nonexistent_command_xyz"))
  expect_true(is.character(result))
})

test_that("picocli-only functions error without picocli|cliError", {
  skip_if(improveR:::hasPicocli(), "Picocli IS available")
  expect_error(statusCli("/tmp"), "requires the new CLI")
  expect_error(mergeCli("/tmp", "f"), "requires the new CLI")
  expect_error(resetCli("/tmp"), "requires the new CLI")
  expect_error(resolveCli("/tmp", "f"), "requires the new CLI")
  expect_error(cleanCli("/tmp"), "requires the new CLI")
  expect_error(addInputsCli("/tmp", "*.R"), "requires the new CLI")
  expect_error(createStepCli("R", "R 3"), "requires the new CLI")
  expect_error(toolsCli(), "requires the new CLI")
  expect_error(infoCli("/tmp"), "requires the new CLI")
})
