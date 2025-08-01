# Shared Token Manager - Simple enhancement with secure secret sharing

#' Get shared token file path
#' 
#' @description Gets the fixed path for shared encrypted tokens
#' @return Character string with path to shared token file
getSharedTokenFilePath <- function() {
  internal_dir <- Sys.getenv("internalFolder", tempdir())
  return(file.path(internal_dir, "shared_tokens.enc"))
}

#' Generate or retrieve shared secret
#' 
#' @description Generates a new shared secret or retrieves existing one from environment/RStudio
#' @param force_new Logical. If TRUE, generates new secret even if one exists
#' @return Character string containing the shared secret UUID
#' @export
getSharedSecret <- function(force_new = FALSE) {
  # Try to get existing secret from environment first
  existing_secret <- Sys.getenv("IMPROVER_SHARED_SECRET", "")
  
  if (!force_new && existing_secret != "") {
    log_info("Using existing shared secret from environment")
    return(existing_secret)
  }
  
  # Generate new secret
  new_secret <- uuid::UUIDgenerate()
  log_info("Generated new shared secret")
  
  # Store in current session environment
  Sys.setenv(IMPROVER_SHARED_SECRET = new_secret)
  
  return(new_secret)
}

#' Set shared secret for current session
#' 
#' @description Sets the shared secret for the current session
#' @param secret The shared secret UUID
#' @export  
setSharedSecret <- function(secret) {
  if (is.null(secret) || secret == "") {
    stop("Valid shared secret is required")
  }
  
  Sys.setenv(IMPROVER_SHARED_SECRET = secret)
  log_info("Set shared secret for current session")
}

#' Check if shared token refresh is running
#' 
#' @description Checks if the shared encrypted token file exists
#' @return Logical indicating if shared tokens are available
#' @export
isSharedTokenRefreshRunning <- function() {
  token_file <- getSharedTokenFilePath()
  return(file.exists(token_file))
}

#' Read shared refreshed tokens
#' 
#' @description Reads and applies tokens from shared encrypted file
#' @param secret Optional. The shared secret. If NULL, gets from environment
#' @return Logical indicating success
#' @export
readSharedRefreshedTokens <- function(secret = NULL) {
  if (is.null(secret)) {
    secret <- Sys.getenv("IMPROVER_SHARED_SECRET", "")
    if (secret == "") {
      log_warn("No shared secret available for reading tokens")
      return(FALSE)
    }
  }
  
  token_file <- getSharedTokenFilePath()
  
  if (!file.exists(token_file)) {
    log_info("Shared token file does not exist:", token_file)
    return(FALSE)
  }
  
  tryCatch({
    # Use your existing XOR decryption
    encrypted <- readLines(token_file)
    decrypted <- xorDecrypt(encrypted, secret)
    tokenList <- jsonlite::fromJSON(decrypted)
    
    # Apply tokens to environment (same as your existing readRefreshed)
    Sys.setenv(IMPROVER_TOKEN = tokenList$IMPROVER_TOKEN)
    Sys.setenv(IMPROVER_TOKEN_EXPIRATION = tokenList$IMPROVER_TOKEN_EXPIRATION)
    Sys.setenv(IMPROVER_REFRESH_TOKEN = tokenList$IMPROVER_REFRESH_TOKEN)
    Sys.setenv(IMPROVER_LAST_ACCESS = tokenList$IMPROVER_LAST_ACCESS)
    
    # Include additional fields if present
    if (!is.null(tokenList$IMPROVER_REPO_URL)) {
      Sys.setenv(IMPROVER_REPO_URL = tokenList$IMPROVER_REPO_URL)
    }
    if (!is.null(tokenList$IMPROVER_USER)) {
      Sys.setenv(IMPROVER_USER = tokenList$IMPROVER_USER)
    }
    
    # Update cached config (same as your existing code)
    if (exists("cacheEnv", envir = globalenv()) && !is.null(cacheEnv$conf)) {
      conf <- cacheEnv$conf
      conf$reqToken <- tokenList$IMPROVER_TOKEN
      cacheEnv$conf <- conf
    }
    
    log_info("Applied shared refreshed tokens to current session")
    return(TRUE)
    
  }, error = function(e) {
    log_error("Failed to read shared refreshed tokens:", e$message)
    return(FALSE)
  })
}

#' Write tokens to shared encrypted file
#' 
#' @description Writes current tokens to shared file using XOR encryption
#' @param secret The shared secret for encryption
#' @return Logical indicating success
writeSharedTokens <- function(secret) {
  if (is.null(secret) || secret == "") {
    stop("Valid shared secret is required")
  }
  
  token_file <- getSharedTokenFilePath()
  
  # Prepare token list (enhanced from your existing logic)
  tokenList <- list(
    IMPROVER_TOKEN = Sys.getenv("IMPROVER_TOKEN"),
    IMPROVER_TOKEN_EXPIRATION = Sys.getenv("IMPROVER_TOKEN_EXPIRATION"),
    IMPROVER_REFRESH_TOKEN = Sys.getenv("IMPROVER_REFRESH_TOKEN"),
    IMPROVER_LAST_ACCESS = Sys.getenv("IMPROVER_LAST_ACCESS"),
    IMPROVER_REPO_URL = Sys.getenv("IMPROVER_REPO_URL"),
    IMPROVER_USER = Sys.getenv("IMPROVER_USER")
  )
  
  tryCatch({
    # Use your existing XOR encryption
    tokenString <- jsonlite::toJSON(tokenList)
    encryptedString <- improveR::xorEncrypt(tokenString, secret)
    
    # Ensure directory exists
    dir.create(dirname(token_file), recursive = TRUE, showWarnings = FALSE)
    
    # Write encrypted tokens
    writeLines(encryptedString, token_file)
    
    log_debug("Wrote shared tokens to:", token_file)
    return(TRUE)
    
  }, error = function(e) {
    log_error("Failed to write shared tokens:", e$message)
    return(FALSE)
  })
}

#' Start shared token refresh manager  
#' 
#' @description Starts background refresh process with shared secret
#' @param secret Optional. Shared secret. If NULL, generates or retrieves one
#' @param verbose Logical. If TRUE, shows process output
#' @return The shared secret used
#' @export
startSharedTokenRefresh <- function(secret = NULL, verbose = FALSE) {
  # Ensure we're connected
  improveConnected()
  
  # Get or generate shared secret
  if (is.null(secret)) {
    secret <- getSharedSecret()
  }
  
  # Set for current session
  setSharedSecret(secret)
  
  # Write initial tokens to shared file
  writeSharedTokens(secret)
  
  # Start background refresh process
  token_file <- getSharedTokenFilePath()
  
  if (verbose) {
    refresh_process <- callr::r_bg(
      func = function(secret, token_file) {
        library(improveR)
        
        # Setup finalizer for cleanup
        env <- new.env()
        reg.finalizer(env, function(x) {
          if (file.exists(token_file)) {
            unlink(token_file, force = TRUE)
          }
        }, onexit = TRUE)
        
        # Refresh loop
        while (file.exists(token_file)) {
          tryCatch({
            print(paste("shared token refresh:", Sys.time()))
            
            # Read current tokens to maintain state
            if (file.exists(token_file)) {
              encrypted <- readLines(token_file)
              decrypted <- improveR:::xorDecrypt(encrypted, secret)
              current_tokens <- jsonlite::fromJSON(decrypted)
              
              # Set environment for renewAccessToken
              Sys.setenv(IMPROVER_REFRESH_TOKEN = current_tokens$IMPROVER_REFRESH_TOKEN)
              if (!is.null(current_tokens$IMPROVER_REPO_URL)) {
                Sys.setenv(IMPROVER_REPO_URL = current_tokens$IMPROVER_REPO_URL)
              }
              
              # Set up authenticationProvider in background process
              if (!exists("cacheEnv", envir = globalenv())) {
                cacheEnv <<- new.env()
              }
              if (is.null(cacheEnv$authenticationProvider)) {
                cacheEnv$authenticationProvider <<- improveR:::getAuthenticationProvider(current_tokens$IMPROVER_REPO_URL)
              }
            }
            
            # Renew tokens using your existing function
            improveR:::renewAccessToken()
            
            # Save updated tokens with encryption
            tokenList <- list(
              IMPROVER_TOKEN = Sys.getenv("IMPROVER_TOKEN"),
              IMPROVER_TOKEN_EXPIRATION = Sys.getenv("IMPROVER_TOKEN_EXPIRATION"),
              IMPROVER_REFRESH_TOKEN = Sys.getenv("IMPROVER_REFRESH_TOKEN"),
              IMPROVER_LAST_ACCESS = Sys.getenv("IMPROVER_LAST_ACCESS"),
              IMPROVER_REPO_URL = Sys.getenv("IMPROVER_REPO_URL"),
              IMPROVER_USER = Sys.getenv("IMPROVER_USER")
            )
            
            tokenString <- jsonlite::toJSON(tokenList)
            encryptedString <- improveR::xorEncrypt(tokenString, secret)
            writeLines(encryptedString, token_file)
            
            print("shared tokens renewed")
            
          }, error = function(e) {
            print(paste("Shared token refresh error:", e$message))
          })
          
          # Keep your existing interval
          Sys.sleep(100)
        }
        
        print("Shared token refresh: file removed, exiting")
      },
      args = list(secret = secret, token_file = token_file),
      stdout = "shared_token_refresh_stdout.txt",
      stderr = "shared_token_refresh_stderr.txt"
    )
  } else {
    refresh_process <- callr::r_bg(
      func = function(secret, token_file) {
        library(improveR)
        
        env <- new.env()
        reg.finalizer(env, function(x) {
          if (file.exists(token_file)) {
            unlink(token_file, force = TRUE)
          }
        }, onexit = TRUE)
        
        while (file.exists(token_file)) {
          tryCatch({
            if (file.exists(token_file)) {
              encrypted <- readLines(token_file)
              decrypted <- improveR:::xorDecrypt(encrypted, secret)
              current_tokens <- jsonlite::fromJSON(decrypted)
              
              Sys.setenv(IMPROVER_REFRESH_TOKEN = current_tokens$IMPROVER_REFRESH_TOKEN)
              if (!is.null(current_tokens$IMPROVER_REPO_URL)) {
                Sys.setenv(IMPROVER_REPO_URL = current_tokens$IMPROVER_REPO_URL)
              }
              
              # Set up authenticationProvider in background process
              if (!exists("cacheEnv", envir = globalenv())) {
                cacheEnv <<- new.env()
              }
              if (is.null(cacheEnv$authenticationProvider)) {
                cacheEnv$authenticationProvider <<- improveR:::getAuthenticationProvider(current_tokens$IMPROVER_REPO_URL)
              }
            }
            
            improveR:::renewAccessToken()
            
            tokenList <- list(
              IMPROVER_TOKEN = Sys.getenv("IMPROVER_TOKEN"),
              IMPROVER_TOKEN_EXPIRATION = Sys.getenv("IMPROVER_TOKEN_EXPIRATION"),
              IMPROVER_REFRESH_TOKEN = Sys.getenv("IMPROVER_REFRESH_TOKEN"),
              IMPROVER_LAST_ACCESS = Sys.getenv("IMPROVER_LAST_ACCESS"),
              IMPROVER_REPO_URL = Sys.getenv("IMPROVER_REPO_URL"),
              IMPROVER_USER = Sys.getenv("IMPROVER_USER")
            )
            
            tokenString <- jsonlite::toJSON(tokenList)
            encryptedString <- improveR::xorEncrypt(tokenString, secret)
            writeLines(encryptedString, token_file)
            
          }, error = function(e) {
            # Silent error handling
          })
          
          Sys.sleep(100)
        }
      },
      args = list(secret = secret, token_file = token_file)
    )
  }
  
  # Store process reference
  assign("shared_token_refresh_process", refresh_process, envir = globalenv())
  
  log_info("Started shared token refresh manager")
  return(secret)
}

#' Stop shared token refresh manager
#' 
#' @description Stops the shared token refresh and cleans up
#' @export
stopSharedTokenRefresh <- function() {
  token_file <- getSharedTokenFilePath()
  
  # Remove token file to signal background process to stop
  if (file.exists(token_file)) {
    unlink(token_file, force = TRUE)
    log_info("Removed shared token file, background process should stop")
  }
  
  # Clean up environment variables
  Sys.setenv(IMPROVER_SHARED_SECRET = "")
  
  # Kill background process if we have reference
  if (exists("shared_token_refresh_process", envir = globalenv())) {
    process <- get("shared_token_refresh_process", envir = globalenv())
    if (!is.null(process) && process$is_alive()) {
      process$kill()
      log_info("Killed shared token refresh process")
    }
    rm("shared_token_refresh_process", envir = globalenv())
  }
}