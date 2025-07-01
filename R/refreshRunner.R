
autoRefreshProcess <- new.env()

autoRefresh <- function() {


  key<- Sys.getenv("IMPROVER_REFRESHKEY")
  path <- Sys.getenv("IMPROVER_KEYPATH")
  authProvider<- improveR:::getAuthenticationProvider(Sys.getenv("IMPROVER_REPO_URL"))
  #key <- uuid::UUIDgenerate()
  env <- new.env()
  reg.finalizer(env,function(x) {improveR:::cleanRefreshFiles()},onexit = TRUE)

  while(TRUE) {
    print("renew")
    improveR:::renewAccessToken()
    tokenList <- list(
      IMPROVER_TOKEN=Sys.getenv("IMPROVER_TOKEN"),
      IMPROVER_TOKEN_EXPIRATION=Sys.getenv("IMPROVER_TOKEN_EXPIRATION"),
      IMPROVER_REFRESH_TOKEN=Sys.getenv("IMPROVER_REFRESH_TOKEN"),
      IMPROVER_LAST_ACCESS=Sys.getenv("IMPROVER_LAST_ACCESS")
    )

    tokenString <- jsonlite::toJSON(tokenList)
    encryptedString <- improveR::xorEncrypt(tokenString,key)
    writeLines(encryptedString,path)
    print("renewed")
    Sys.sleep(100)
  }

}



readRefreshed <- function() {
  key<- Sys.getenv("IMPROVER_REFRESHKEY")
  path <- Sys.getenv("IMPROVER_KEYPATH")
  encrypted <- readLines(path)
  decrypted <- xorDecrypt(encrypted,key)
  tokenList <- jsonlite::fromJSON(decrypted)
  Sys.setenv(IMPROVER_TOKEN=tokenList$IMPROVER_TOKEN)
  Sys.setenv(IMPROVER_TOKEN_EXPIRATION=tokenList$IMPROVER_TOKEN_EXPIRATION)
  Sys.setenv(IMPROVER_REFRESH_TOKEN=tokenList$IMPROVER_REFRESH_TOKEN)
  Sys.setenv(IMPROVER_LAST_ACCESS=tokenList$IMPROVER_LAST_ACCESS)
  conf <- cacheEnv$conf
  conf$reqToken <- tokenList$IMPROVER_TOKEN
  cacheEnv$conf <- conf
}

cleanRefreshFiles <- function() {
  keyPath <- Sys.getenv("IMPROVER_KEYPATH")
  if (keyPath!=""){
    unlink(keyPath,force=T)
  }
}

#' autoRefreshStart
#'
#' starts a background process to automatically refresh the token
#'
#' @export
autoRefreshStart <- function(verbose=F) {




  improveConnected()
  refreshKey <- uuid::UUIDgenerate()
  Sys.setenv(IMPROVER_REFRESHKEY=refreshKey)
  Sys.setenv(IMPROVER_KEYPATH=file.path(getwd(),openssl::sha256(refreshKey)))
  if (verbose) {
    autoRefreshProcess$autoRefreshProcess <- callr::r_bg(improveR:::autoRefresh,stdout = "stdout.txt",stderr="stderr.txt")
  } else {
    autoRefreshProcess$autoRefreshProcess <- callr::r_bg(improveR:::autoRefresh)
  }

}

#' autoRefreshRunning
#'
#' checks is autoRefresh is running
#'
#' @export
autoRefreshRunning <- function() {
  path <- Sys.getenv("IMPROVER_KEYPATH")
  return(path!="" && file.exists(path))
}


#' autoRefreshStartedByMe
#'
#' checks is autoRefresh was started by this process
#'
#' @export
autoRefreshStartedByMe <- function() {
  return(!is.null(autoRefreshProcess$autoRefreshProcess) && autoRefreshProcess$autoRefreshProcess$is_alive())
}



#' autoRefreshStop
#'
#' stops the background process to automatically refresh the token
#'
#' @export
autoRefreshStop <- function() {
  if (autoRefreshRunning()) {
    Sys.setenv(IMPROVER_REFRESHKEY="")
    Sys.setenv(IMPROVER_KEYPATH="")
    autoRefreshProcess$autoRefreshProcess$kill()
  }
}
