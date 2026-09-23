# Token Refresher Plugin Architecture
# Allows pluggable token refresh mechanisms

# Token refresher registry
.tokenRefresherRegistry <- new.env(parent = emptyenv())

#' Register a token refresher plugin
#' 
#' @description Registers a token refresher implementation
#' @param name Character string. Name of the refresher
#' @param refresher List with required methods: init, start, stop, isRunning, getToken
#' @returns No meaningful value - called for its side effect of putting the refresher
#'   into the registry under `name`. Stops with an error when `refresher` is not a list, or
#'   when it does not carry all five of `init`, `start`, `stop`, `isRunning` and
#'   `getToken`. Registering a name twice replaces the earlier entry.
#' @export
registerTokenRefresher <- function(name, refresher) {
  required_methods <- c("init", "start", "stop", "isRunning", "getToken")
  
  if (!is.list(refresher)) {
    stop("Token refresher must be a list with required methods")
  }
  
  missing_methods <- setdiff(required_methods, names(refresher))
  if (length(missing_methods) > 0) {
    stop("Token refresher missing required methods: ", paste(missing_methods, collapse = ", "))
  }
  
  .tokenRefresherRegistry[[name]] <- refresher
  log_info("Registered token refresher:", name)
}

#' Get registered token refresher
#' 
#' @description Gets a registered token refresher by name
#' @param name Character string. Name of the refresher
#' @return Token refresher object or NULL if not found
#' @export
getTokenRefresher <- function(name) {
  .tokenRefresherRegistry[[name]]
}

#' List registered token refreshers
#' 
#' @description Lists all registered token refresher names
#' @return Character vector of refresher names
#' @export
listTokenRefreshers <- function() {
  names(.tokenRefresherRegistry)
}

#' Get active token refresher
#' 
#' @description Gets the currently active token refresher
#' @return Token refresher object or NULL
#' @export
getActiveTokenRefresher <- function() {
  # Check environment variable for preference
  preferred <- Sys.getenv("IMPROVER_TOKEN_REFRESHER", "")
  
  if (preferred != "" && !is.null(.tokenRefresherRegistry[[preferred]])) {
    return(.tokenRefresherRegistry[[preferred]])
  }
  
  # Check for channel-based refresher first (production)
  if (!is.null(.tokenRefresherRegistry[["channel"]])) {
    return(.tokenRefresherRegistry[["channel"]])
  }
  
  # Fall back to file-based refresher (testing)
  if (!is.null(.tokenRefresherRegistry[["file"]])) {
    return(.tokenRefresherRegistry[["file"]])
  }
  
  return(NULL)
}

# Token refresher implementations should be registered by the packages that provide them
# This file only contains the plugin infrastructure