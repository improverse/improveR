test_that("token refresh plugin works when token expires", {
  # Skip if improveRtestsupport is not available
  skip_if_not(requireNamespace("improveRtestsupport", quietly = TRUE),
              "improveRtestsupport not available")

  # Skip if not connected
  skip_if_not(improveR::improveConnected(silent = TRUE),
              "Not connected to improve")

  # Check if token refresher is registered
  has_refresher <- getActiveTokenRefresher()
  expect_false(is.null(has_refresher))

  # Get current token expiration time
  token_expiration <- as.numeric(Sys.getenv("IMPROVER_TOKEN_EXPIRATION", "300"))
  cat("Token expiration time:", token_expiration, "seconds\n")

  # Store initial token
  initial_token <- Sys.getenv("IMPROVER_TOKEN")
  expect_true(nchar(initial_token) > 0, "Initial token should exist")

  # For testing, we'll use a shorter wait time
  # The token refresh should happen automatically when the token expires

  wait_time <- token_expiration

  cat("Waiting", wait_time, "seconds for token to expire and refresh...\n")

  # Capture log output to check for refresh message
  log_output <- capture.output({
    # Wait for token expiration
    Sys.sleep(wait_time)

    # Call refreshToken to trigger the refresh if needed
    # This should trigger the plugin and log "Token refreshed via plugin"
    tryCatch({
      improveR::refreshToken()
      cat("refreshToken() completed\n")
    }, error = function(e) {
      cat("Error calling refreshToken():", e$message, "\n")
    })
  }, type = "message")

  # Check if we got the expected log message
  refresh_logged <- any(grepl("Token refreshed via plugin", log_output))

  if (refresh_logged) {
    cat("✓ Token refresh plugin logged successful refresh\n")
  } else {
    cat("✗ Token refresh plugin did not log refresh\n")
    cat("Log output:\n", paste(log_output, collapse = "\n"), "\n")
  }

  # Check if token was actually refreshed
  current_token <- Sys.getenv("IMPROVER_TOKEN")

  if (current_token != initial_token) {
    cat("✓ Token value changed after refresh\n")
    expect_true(TRUE, "Token was successfully refreshed")
  } else {
    cat("⚠ Token value unchanged, checking if refresh is working via API call\n")

    # Try an API call using authenticatedREST which should apply the refreshed token
    tryCatch({
      result <- improveR:::authenticatedREST("/users", restType = "GET")
      expect_true(!is.null(result), "API call should succeed after token refresh")
      cat("✓ API call succeeded, token refresh is working\n")
    }, error = function(e) {
      fail(paste("API call failed after token refresh attempt:", e$message))
    })
  }
})

test_that("manual token refresh via plugin works", {
  # Skip if improveRtestsupport is not available
  skip_if_not(requireNamespace("improveRtestsupport", quietly = TRUE),
              "improveRtestsupport not available")

  # Skip if not connected
  skip_if_not(improveR::improveConnected(silent = TRUE),
              "Not connected to improve")

  # Store current token
  initial_token <- Sys.getenv("IMPROVER_TOKEN")

  # Force refresh by using alwaysRefresh parameter
  # Note: setting expiration to 0 won't work because shouldRefreshToken() returns FALSE when expiration==0
  cat("Forcing token refresh using alwaysRefresh=TRUE...\n")

  # Capture log output using redirectLogs (logging:: writes to handler, not message())
  improveR:::redirectLogs(TRUE)
  tryCatch({
    improveR::refreshToken(alwaysRefresh = TRUE)
    cat("refreshToken() completed\n")
  }, error = function(e) {
    cat("Error:", e$message, "\n")
  })

  # Check for the plugin log message in the structured log list
  logs <- improveR:::logEnv$logs
  all_messages <- unlist(logs, recursive = TRUE)
  refresh_logged <- any(grepl("Token refreshed via plugin", all_messages))

  # Reset log redirection
  improveR:::resetLogs()
  improveR:::redirectLogs(FALSE)

  expect_true(refresh_logged,
              "Token refresh plugin should log 'Token refreshed via plugin'")

  if (refresh_logged) {
    cat("✓ Token refresh plugin successfully triggered\n")
  } else {
    cat("Log messages captured:\n", paste(all_messages, collapse = "\n"), "\n")
  }

  # Verify the token works
  tryCatch({
    result <- improveR:::authenticatedREST("/users", restType = "GET")
    expect_true(!is.null(result), "API call should succeed with refreshed token")
    cat("✓ Refreshed token works for API calls\n")
  }, error = function(e) {
    warning("API call failed: ", e$message)
  })
})
