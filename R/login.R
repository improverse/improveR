


#' refreshToken
#' @description refreshToken requests a new token to access the system.
#' Run tokens do not need to be refreshed.
#' @param alwaysRefresh refresh no matter how much time has elapsed
#' @references ics1208
#' @seealso [improveConnect()]
#' @export
refreshToken <- function(alwaysRefresh=F) {
  token <- Sys.getenv("IMPROVER_REFRESH_TOKEN")
  if (token!="") {
    expiration <- Sys.getenv("IMPROVER_TOKEN_EXPIRATION")
    if (expiration!="") {
      expirationSeconds <- as.numeric(expiration)
      lastAccess <- as.numeric(Sys.getenv("IMPROVER_LAST_ACCESS"))
      timeDiff <- as.numeric(Sys.time())-lastAccess
      if (timeDiff > (expirationSeconds/2) || alwaysRefresh) {

        # Update last access time
        Sys.setenv(IMPROVER_LAST_ACCESS=as.numeric(Sys.time()))
        
        # Try plugin-based token refresher first
        refresher <- getActiveTokenRefresher()
        if (!is.null(refresher)) {
          if (refresher$isRunning()) {
            tokenData <- refresher$getToken()
            if (!is.null(tokenData)) {
              log_info("Used plugin-based token refresh system")
              return()
            }
          }
        }
        
        # Legacy: Try shared token system if available
        if (exists("isSharedTokenRefreshRunning", mode = "function")) {
          if (isSharedTokenRefreshRunning()) {
            if (readSharedRefreshedTokens()) {
              log_info("Used legacy shared token refresh system")
              return()
            }
          }
        }
        
        # Direct token refresh if no refresh system running
        log_info("refreshing token directly")
        renewAccessToken()
      }
    }
  }
}

