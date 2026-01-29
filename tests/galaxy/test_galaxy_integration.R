#!/usr/bin/env Rscript
# Galaxy Hub Integration Test
#
# This test verifies that:
# 1. improveR can authenticate and get tokens
# 2. Galaxy Hub can manage those tokens
# 3. A child process can use improveR with Galaxy-managed tokens
#
# Prerequisites:
# - Galaxy Hub running on port 1408
# - Valid OAuth credentials (via .Renviron or improveRtestsupport)
#
# Usage:
#   cd /mnt/c/dev/git-repos/improVerse/improveR
#   R_LIBS_USER=~/R/library Rscript tests/galaxy/test_galaxy_integration.R

# Load .Renviron if present
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

# Set auto auth
Sys.setenv(IMPROVER_AUTO_AUTH = "TRUE")
Sys.setenv(IMPROVER_HEADLESS_OAUTH = "TRUE")

cat("=== Galaxy Hub Integration Test ===\n\n")

# Load libraries
library(improveR)
library(improveRtestsupport)
library(improVerticles)

# Step 1: Authenticate
cat("Step 1: Authenticating with improveR...\n")
improveR::clearConnectionData()
improveRtestsupport::improveConnect()

token <- Sys.getenv("IMPROVER_TOKEN", "")
if (!nzchar(token)) {
  stop("Authentication failed - no token")
}
cat("  Authenticated as:", Sys.getenv("IMPROVER_USER"), "\n")
cat("  Repo:", Sys.getenv("IMPROVER_REPO_URL"), "\n\n")

# Step 2: Connect to Galaxy Hub
cat("Step 2: Connecting to Galaxy Hub...\n")
improVerticles::galaxyConfig(host = "127.0.0.1", port = 1408)
connected <- improVerticles::galaxyConnect(wait = TRUE, timeout = 5)
if (!connected) {
  stop("Failed to connect to Galaxy Hub. Is it running on port 1408?")
}
cat("  Connected to Galaxy Hub\n")

# Step 3: Initialize session
cat("Step 3: Initializing Galaxy session...\n")
result <- improVerticles::galaxyInitFromEnv()
if (!isTRUE(result$success)) {
  stop("Failed to init session: ", result$error)
}
session_key <- result$sessionKey
shared_secret <- result$sharedSecret
cat("  Session:", session_key, "\n\n")

# Step 4: Create and start child process
cat("Step 4: Starting child process with Galaxy token management...\n")

# Create child script
child_script <- tempfile(fileext = ".R")
writeLines(c(
  '# Child process - tests Galaxy token injection',
  'cat("\\n=== Child Process Started ===\\n\\n")',
  '',
  '# Environment check',
  'cat("Environment Variables:\\n")',
  'cat("  GALAXY_HOST:", Sys.getenv("GALAXY_HOST", "<not set>"), "\\n")',
  'cat("  GALAXY_PORT:", Sys.getenv("GALAXY_PORT", "<not set>"), "\\n")',
  'cat("  IMPROVER_TOKEN_REFRESHER:", Sys.getenv("IMPROVER_TOKEN_REFRESHER", "<not set>"), "\\n")',
  'cat("  R_PROFILE_USER:", Sys.getenv("R_PROFILE_USER", "<not set>"), "\\n")',
  '',
  '# Check token',
  'token <- Sys.getenv("IMPROVER_TOKEN", "")',
  'cat("\\nToken Status:\\n")',
  'if (nzchar(token)) {',
  '  cat("  Token available:", nchar(token), "chars\\n")',
  '  cat("  User:", Sys.getenv("IMPROVER_USER", "unknown"), "\\n")',
  '} else {',
  '  cat("  WARNING: No token available!\\n")',
  '}',
  '',
  '# Test improveR API call',
  'if (nzchar(token)) {',
  '  cat("\\nTesting improveR API call...\\n")',
  '  tryCatch({',
  '    library(improveR)',
  '    user <- improveR::getUser()',
  '    cat("  SUCCESS: getUser() returned:", user, "\\n")',
  '  }, error = function(e) {',
  '    cat("  ERROR:", e$message, "\\n")',
  '  })',
  '}',
  '',
  'cat("\\n=== Child Process Complete ===\\n")'
), child_script)

cat("  Child script:", child_script, "\n")

# Send startProcess request
workdir <- getwd()
json <- sprintf(
  '{"action":"startProcess","toolId":"local-rscript-improver","workdir":"%s","scriptFile":"%s","sessionKey":"%s","sharedSecret":"%s"}',
  gsub("\\\\", "/", workdir),
  gsub("\\\\", "/", child_script),
  session_key,
  shared_secret
)

# Set up handlers for process events
process_complete <- FALSE

improVerticles::galaxyOnMessage("processStarted", function(msg) {
  cat("  Process started: ", msg$processId, " (PID: ", msg$pid, ")\n", sep = "")
})

improVerticles::galaxyOnMessage("processOutput", function(msg) {
  cat("  [", msg$stream, "] ", msg$data, "\n", sep = "")
})

improVerticles::galaxyOnMessage("processCompleted", function(msg) {
  cat("\n  Process completed with exit code: ", msg$exitCode, "\n", sep = "")
  process_complete <<- TRUE
})

improVerticles::galaxyOnMessage("error", function(msg) {
  cat("  ERROR: ", msg$message, "\n", sep = "")
  process_complete <<- TRUE
})

# Send the request
improVerticles:::.galaxy$ws$send(json)
cat("  Request sent, waiting for output...\n\n")

# Wait for process to complete (max 30 seconds)
start_time <- Sys.time()
while (!process_complete && difftime(Sys.time(), start_time, units = "secs") < 30) {
  later::run_now(timeoutSecs = 0.1)
}

if (!process_complete) {
  cat("  TIMEOUT: Process did not complete within 30 seconds\n")
}

# Cleanup
unlink(child_script)
improVerticles::galaxyDisconnect()

cat("\n=== Test Complete ===\n")
