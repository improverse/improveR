# Token Refresher Plugin Architecture
# Allows pluggable token refresh mechanisms

# Token refresher registry
.tokenRefresherRegistry <- new.env(parent = emptyenv())

#' Register a token refresher plugin
#' 
#' @description Registers a token refresher implementation
#' @param name Character string. Name of the refresher
#' @param refresher List with required methods: init, start, stop, isRunning, getToken
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

#' Create file-based token refresher
#' 
#' @description Creates a file-based token refresher using XOR encryption
#' @return Token refresher object
#' @export
createFileTokenRefresher <- function() {
  list(
    init = function(config = list()) {
      # Initialize file-based refresher
      if (!requireNamespace("improveRtestsupport", quietly = TRUE)) {
        stop("improveRtestsupport package required for file-based token refresh")
      }
      log_info("File-based token refresher initialized")
    },
    
    start = function(secret = NULL, verbose = FALSE) {
      improveRtestsupport::startSharedTokenRefresh(secret, verbose)
    },
    
    stop = function() {
      improveRtestsupport::stopSharedTokenRefresh()
    },
    
    isRunning = function() {
      improveRtestsupport::isSharedTokenRefreshRunning()
    },
    
    getToken = function(secret = NULL) {
      if (improveRtestsupport::readSharedRefreshedTokens(secret)) {
        return(list(
          token = Sys.getenv("IMPROVER_TOKEN"),
          expiration = Sys.getenv("IMPROVER_TOKEN_EXPIRATION"),
          refresh_token = Sys.getenv("IMPROVER_REFRESH_TOKEN")
        ))
      }
      return(NULL)
    }
  )
}

#' Create channel-based token refresher
#' 
#' @description Creates a channel-based token refresher using encrypted channels
#' @return Token refresher object
#' @export
createChannelTokenRefresher <- function() {
  # Private state
  env <- new.env(parent = emptyenv())
  env$verticle <- NULL
  env$sessionKey <- NULL
  env$sharedSecret <- NULL
  
  list(
    init = function(config = list()) {
      # Initialize channel-based refresher
      if (!requireNamespace("improVerticles", quietly = TRUE)) {
        stop("improVerticles package required for channel-based token refresh")
      }
      
      # Get or create session key
      env$sessionKey <- config$sessionKey
      if (is.null(env$sessionKey)) {
        env$sessionKey <- Sys.getenv("IMPROVER_SESSION_KEY", "")
        if (env$sessionKey == "") {
          env$sessionKey <- uuid::UUIDgenerate()
          Sys.setenv(IMPROVER_SESSION_KEY = env$sessionKey)
        }
      }
      
      # Get or create shared secret
      env$sharedSecret <- config$sharedSecret
      if (is.null(env$sharedSecret)) {
        envSecret <- Sys.getenv("IMPROVER_SHARED_SECRET", "")
        if (nzchar(envSecret)) {
          env$sharedSecret <- openssl::base64_decode(envSecret)
        } else {
          env$sharedSecret <- openssl::rand_bytes(32)
          Sys.setenv(IMPROVER_SHARED_SECRET = openssl::base64_encode(env$sharedSecret))
        }
      }
      
      # Get the token refresher verticle
      env$verticle <- improVerticles::getImproVerticle("com.scinteco.auth.tokenRefresher-1.0.0")
      if (is.null(env$verticle)) {
        stop("Token refresher verticle not found. Please install com.scinteco.auth.tokenRefresher-1.0.0")
      }
      
      # Initialize the verticle
      env$verticle$init(env$sessionKey, env$sharedSecret)
      
      log_info("Channel-based token refresher initialized")
    },
    
    start = function(secret = NULL, verbose = FALSE) {
      if (is.null(env$verticle)) {
        stop("Token refresher not initialized. Call init() first.")
      }
      
      if (verbose && !is.null(env$verticle$setVerbose)) {
        env$verticle$setVerbose(TRUE)
      }
      
      env$verticle$start()
      log_info("Started channel-based token refresh")
    },
    
    stop = function() {
      if (!is.null(env$verticle)) {
        env$verticle$stop()
        log_info("Stopped channel-based token refresh")
      }
    },
    
    isRunning = function() {
      if (!is.null(env$verticle) && !is.null(env$verticle$isRunning)) {
        return(env$verticle$isRunning)
      }
      return(FALSE)
    },
    
    getToken = function(secret = NULL) {
      if (is.null(env$verticle)) {
        return(NULL)
      }
      
      # Request token via channel
      requestSender <- improVerticles::getImproVerticle("com.scinteco.galaxy.sender-1.0.0")
      requestSender$setSessionKey(env$sessionKey)
      requestSender$setSharedSecret(env$sharedSecret)
      requestSender$init("requestToken")
      
      responseReceiver <- improVerticles::getImproVerticle("com.scinteco.galaxy.receiver-1.0.0")
      responseReceiver$setSessionKey(env$sessionKey)
      responseReceiver$setSharedSecret(env$sharedSecret)
      responseReceiver$init("tokenResponse")
      
      # Send request
      requestSender$send("getToken")
      
      # Wait for response (with timeout)
      max_wait <- 5  # seconds
      start_time <- Sys.time()
      
      while (as.numeric(Sys.time() - start_time) < max_wait) {
        message <- responseReceiver$getMessage()
        if (!is.null(message)) {
          tokenData <- jsonlite::fromJSON(message)
          
          # Apply to environment
          Sys.setenv(IMPROVER_TOKEN = tokenData$IMPROVER_TOKEN)
          Sys.setenv(IMPROVER_TOKEN_EXPIRATION = tokenData$IMPROVER_TOKEN_EXPIRATION)
          Sys.setenv(IMPROVER_REFRESH_TOKEN = tokenData$IMPROVER_REFRESH_TOKEN)
          Sys.setenv(IMPROVER_LAST_ACCESS = tokenData$IMPROVER_LAST_ACCESS)
          
          if (!is.null(tokenData$IMPROVER_REPO_URL)) {
            Sys.setenv(IMPROVER_REPO_URL = tokenData$IMPROVER_REPO_URL)
          }
          if (!is.null(tokenData$IMPROVER_USER)) {
            Sys.setenv(IMPROVER_USER = tokenData$IMPROVER_USER)
          }
          
          # Update cached config
          if (exists("cacheEnv", envir = globalenv()) && !is.null(cacheEnv$conf)) {
            conf <- cacheEnv$conf
            conf$reqToken <- tokenData$IMPROVER_TOKEN
            cacheEnv$conf <- conf
          }
          
          log_info("Retrieved token via encrypted channel")
          
          return(list(
            token = tokenData$IMPROVER_TOKEN,
            expiration = tokenData$IMPROVER_TOKEN_EXPIRATION,
            refresh_token = tokenData$IMPROVER_REFRESH_TOKEN
          ))
        }
        Sys.sleep(0.1)
      }
      
      log_warn("Timeout waiting for token response")
      return(NULL)
    }
  )
}

#' Initialize default token refreshers
#' 
#' @description Registers the default token refresher implementations
#' @export
initializeTokenRefreshers <- function() {
  # Register file-based refresher
  tryCatch({
    registerTokenRefresher("file", createFileTokenRefresher())
  }, error = function(e) {
    log_debug("Could not register file-based token refresher:", e$message)
  })
  
  # Register channel-based refresher
  tryCatch({
    registerTokenRefresher("channel", createChannelTokenRefresher())
  }, error = function(e) {
    log_debug("Could not register channel-based token refresher:", e$message)
  })
}