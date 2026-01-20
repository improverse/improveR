#!/usr/bin/env Rscript
# Galaxy Hub Shiny App Test
#
# This test:
# 1. Authenticates with improveR
# 2. Initializes a Galaxy session
# 3. Starts a Shiny app via Galaxy Hub
# 4. Captures the port from stdout
# 5. Opens the app URL
#
# Usage:
#   cd /mnt/c/dev/git-repos/improVerse/improveR
#   R_LIBS_USER=~/R/library Rscript tests/galaxy/test_shiny_app.R

# Load .Renviron if present
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

Sys.setenv(IMPROVER_AUTO_AUTH = "TRUE")
Sys.setenv(IMPROVER_HEADLESS_OAUTH = "TRUE")

cat("=== Galaxy Hub Shiny App Test ===\n\n")

library(improveR)
library(improveRtestsupport)
library(galaxyR)

# Step 1: Authenticate
cat("Step 1: Authenticating...\n")
improveR::clearConnectionData()
improveRtestsupport::improveConnect()

token <- Sys.getenv("IMPROVER_TOKEN", "")
if (!nzchar(token)) {
  stop("Authentication failed")
}
cat("  Authenticated as:", Sys.getenv("IMPROVER_USER"), "\n\n")

# Step 2: Connect to Galaxy Hub
cat("Step 2: Connecting to Galaxy Hub...\n")
galaxyR::galaxyConfig(host = "127.0.0.1", port = 1408)
connected <- galaxyR::galaxyConnect(wait = TRUE, timeout = 5)
if (!connected) {
  stop("Failed to connect to Galaxy Hub")
}
cat("  Connected\n")

# Step 3: Initialize session
cat("Step 3: Initializing session...\n")
result <- galaxyR::galaxyInitFromEnv()
if (!isTRUE(result$success)) {
  stop("Failed to init session: ", result$error)
}
session_key <- result$sessionKey
shared_secret <- result$sharedSecret
cat("  Session:", session_key, "\n\n")

# Step 4: Start Shiny app
cat("Step 4: Starting Shiny app...\n")

shiny_script <- "/mnt/c/dev/git-repos/improverticles/galaxy-hub/test/scripts/minimal_shiny_app.R"
workdir <- "/mnt/c/dev/git-repos/improverticles/galaxy-hub/test"

json <- sprintf(
  '{"action":"startProcess","toolId":"local-rscript-improver","workdir":"%s","scriptFile":"%s","sessionKey":"%s","sharedSecret":"%s"}',
  workdir,
  shiny_script,
  session_key,
  shared_secret
)

# Track process output to find port
shiny_port <- NULL
process_id <- NULL

galaxyR::galaxyOnMessage("processStarted", function(msg) {
  process_id <<- msg$processId
  cat("  Process started:", msg$processId, "(PID:", msg$pid, ")\n")
})

galaxyR::galaxyOnMessage("processOutput", function(msg) {
  # Look for Shiny's "Listening on" message
  if (grepl("Listening on http://[^:]+:(\\d+)", msg$data)) {
    port_match <- regmatches(msg$data, regexec("Listening on http://[^:]+:(\\d+)", msg$data))
    if (length(port_match[[1]]) > 1) {
      shiny_port <<- as.integer(port_match[[1]][2])
      cat("\n  *** SHINY PORT DETECTED:", shiny_port, "***\n\n")
    }
  }
  cat("  [", msg$stream, "] ", msg$data, "\n", sep = "")
})

galaxyR::galaxyOnMessage("processCompleted", function(msg) {
  cat("\n  Process exited with code:", msg$exitCode, "\n")
})

galaxyR::galaxyOnMessage("error", function(msg) {
  cat("  ERROR:", msg$message, "\n")
})

# Listen for shinyPort message sent directly via WebSocket
galaxyR::galaxyOnMessage("shinyPort", function(msg) {
  if (!is.null(msg$port)) {
    shiny_port <<- as.integer(msg$port)
    cat("\n  *** SHINY PORT RECEIVED VIA WEBSOCKET:", shiny_port, "***\n\n")
  }
})

# Send request
galaxyR:::.galaxy$ws$send(json)
cat("  Request sent, waiting for Shiny to start...\n\n")

# Wait for port to be detected (max 30 seconds)
# Check multiple sources: WebSocket messages, process output, and port file
port_files <- c("/tmp/.shiny_port", file.path(workdir, ".shiny_port"))
start_time <- Sys.time()
while (is.null(shiny_port) && difftime(Sys.time(), start_time, units = "secs") < 30) {
  # Check WebSocket messages
  later::run_now(timeoutSecs = 0.5)

  # Also check port files as fallback
  for (port_file in port_files) {
    if (is.null(shiny_port) && file.exists(port_file)) {
      port_content <- tryCatch(readLines(port_file, warn = FALSE)[1], error = function(e) NA)
      if (!is.na(port_content) && nzchar(port_content)) {
        shiny_port <- as.integer(port_content)
        cat("\n  *** SHINY PORT FROM FILE:", shiny_port, "(", port_file, ")***\n\n")
        break
      }
    }
  }
}

if (!is.null(shiny_port)) {
  url <- sprintf("http://127.0.0.1:%d", shiny_port)
  cat("\n=== Shiny App Running ===\n")
  cat("URL:", url, "\n")
  cat("Process ID:", process_id, "\n")
  cat("\nPress Ctrl+C to stop monitoring. The app will continue running.\n\n")

  # Keep monitoring output
  cat("--- Shiny App Output ---\n")
  tryCatch({
    while (TRUE) {
      later::run_now(timeoutSecs = 1)
    }
  }, interrupt = function(e) {
    cat("\n\nMonitoring stopped. App may still be running.\n")
  })
} else {
  cat("\nFailed to detect Shiny port within 30 seconds.\n")
  cat("Check Galaxy Hub logs for more details.\n")
}

cat("\n=== Test Complete ===\n")
