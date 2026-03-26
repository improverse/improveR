# Token Management Functions
# Provides functions to update tokens in the current session

#' Update access token in current session
#'
#' @description Updates the access token in both environment variables and cached configuration
#' @param token Character. The new access token
#' @param expiration Character or numeric. Token expiration time
#' @param refreshToken Character. Optional refresh token
#' @param lastAccess Character or numeric. Optional last access time
#' @export
updateAccessToken <- function(token, expiration = NULL, refreshToken = NULL, lastAccess = NULL) {
  if (is.null(token) || token == "") {
    stop("Valid token is required")
  }

  # Update environment variables
  Sys.setenv(IMPROVER_TOKEN = token)

  if (!is.null(expiration)) {
    Sys.setenv(IMPROVER_TOKEN_EXPIRATION = as.character(expiration))
  }

  if (!is.null(refreshToken)) {
    Sys.setenv(IMPROVER_REFRESH_TOKEN = refreshToken)
  }

  if (!is.null(lastAccess)) {
    Sys.setenv(IMPROVER_LAST_ACCESS = as.character(lastAccess))
  } else {
    Sys.setenv(IMPROVER_LAST_ACCESS = as.character(as.numeric(Sys.time())))
  }

  # Update cached configuration
  if (!is.null(cacheEnv$conf)) {
    conf <- cacheEnv$conf
    conf$reqToken <- token
    cacheEnv$conf <- conf
    log_debug("Updated cached token configuration")
  }

  invisible(TRUE)
}

#' Apply token data to session
#'
#' @description Applies a complete token data structure to the current session
#' @param tokenData List containing token information with fields:
#'   IMPROVER_TOKEN, IMPROVER_TOKEN_EXPIRATION, IMPROVER_REFRESH_TOKEN,
#'   IMPROVER_LAST_ACCESS, IMPROVER_REPO_URL, IMPROVER_USER
#' @export
applyTokenData <- function(tokenData) {
  if (!is.list(tokenData)) {
    stop("tokenData must be a list")
  }

  # Apply each field if present
  if (!is.null(tokenData$IMPROVER_TOKEN)) {
    Sys.setenv(IMPROVER_TOKEN = tokenData$IMPROVER_TOKEN)
  }

  if (!is.null(tokenData$IMPROVER_TOKEN_EXPIRATION)) {
    Sys.setenv(IMPROVER_TOKEN_EXPIRATION = as.character(tokenData$IMPROVER_TOKEN_EXPIRATION))
  }

  if (!is.null(tokenData$IMPROVER_REFRESH_TOKEN)) {
    Sys.setenv(IMPROVER_REFRESH_TOKEN = tokenData$IMPROVER_REFRESH_TOKEN)
  }

  if (!is.null(tokenData$IMPROVER_LAST_ACCESS)) {
    Sys.setenv(IMPROVER_LAST_ACCESS = as.character(tokenData$IMPROVER_LAST_ACCESS))
  }

  if (!is.null(tokenData$IMPROVER_REPO_URL)) {
    Sys.setenv(IMPROVER_REPO_URL = tokenData$IMPROVER_REPO_URL)
  }

  if (!is.null(tokenData$IMPROVER_USER)) {
    Sys.setenv(IMPROVER_USER = tokenData$IMPROVER_USER)
  }

  # Update cached configuration
  if (!is.null(cacheEnv$conf) && !is.null(tokenData$IMPROVER_TOKEN)) {
    conf <- cacheEnv$conf
    conf$reqToken <- tokenData$IMPROVER_TOKEN
    cacheEnv$conf <- conf
    log_debug("Updated cached token configuration from token data")
  }

  invisible(TRUE)
}

#' Refresh token using registered refresher
#'
#' @description Refreshes the access token using the active token refresher plugin
#' @param alwaysRefresh Logical. If TRUE, refreshes even if token hasn't expired
#' @return Logical indicating success
#' @export
refreshToken <- function(alwaysRefresh = FALSE) {
  # Check if we should refresh
  if (!alwaysRefresh && !shouldRefreshToken()) {
    return(invisible(TRUE))
  }

  # Try plugin-based refresh first
  refresher <- getActiveTokenRefresher()
  if (!is.null(refresher)) {
    tryCatch({
      tokenData <- refresher$getToken()
      if (!is.null(tokenData)) {
        applyTokenData(tokenData)
        log_info("Token refreshed via plugin")
        return(invisible(TRUE))
      }
    }, error = function(e) {
      log_debug("Plugin refresh failed:", e$message)
    })
  }

  # Fall back to OAuth refresh if available
  refreshToken <- Sys.getenv("IMPROVER_REFRESH_TOKEN", "")
  if (nzchar(refreshToken)) {
    tryCatch({
      renewAccessToken()
      log_info("Token refreshed via OAuth")
      return(invisible(TRUE))
    }, error = function(e) {
      log_error("OAuth refresh failed:", e$message)
    })
  }

  log_warn("No token refresh method available")
  return(invisible(FALSE))
}

#' Check if token should be refreshed
#'
#' @description Checks if the current token should be refreshed based on expiration
#' @return Logical indicating if refresh is needed
#' @export
shouldRefreshToken <- function() {
  expiration <- as.numeric(Sys.getenv("IMPROVER_TOKEN_EXPIRATION", "0"))
  lastAccess <- as.numeric(Sys.getenv("IMPROVER_LAST_ACCESS", "0"))

  if (expiration == 0 || lastAccess == 0) {
    return(FALSE)
  }

  timeDiff <- as.numeric(Sys.time()) - lastAccess

  # Refresh if more than half the token lifetime has passed
  return(timeDiff > (expiration / 2))
}

#' Get current token data
#'
#' @description Gets the current token data from environment variables
#' @return List with token data or NULL if no token
#' @export
getCurrentTokenData <- function() {
  token <- Sys.getenv("IMPROVER_TOKEN", "")
  if (token == "") {
    return(NULL)
  }

  list(
    IMPROVER_TOKEN = token,
    IMPROVER_TOKEN_EXPIRATION = Sys.getenv("IMPROVER_TOKEN_EXPIRATION", ""),
    IMPROVER_REFRESH_TOKEN = Sys.getenv("IMPROVER_REFRESH_TOKEN", ""),
    IMPROVER_LAST_ACCESS = Sys.getenv("IMPROVER_LAST_ACCESS", ""),
    IMPROVER_REPO_URL = Sys.getenv("IMPROVER_REPO_URL", ""),
    IMPROVER_USER = Sys.getenv("IMPROVER_USER", ""),
    timestamp = as.character(Sys.time())
  )
}
