#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


codeVerifier <- NULL

#' improveOAuth
#'
#' @description improveOAuth is used to connect to the repository via oauth.
#' @param repo the repository URL in this form https://<url>:<port>/<repositoryPath> the api part (/api/v1) is added automatically
#' @param shortEntityId one valid short entity ID must be provided, this is used as pathWorkingDirectory
#' @param logLevel possible LogLevels: DEBUG, INFO, WARN, ERROR
#' @param secure if TRUE the certificates are checked.
#' Default is TRUE, it can be set to false also by the environment variable IMPROVER_SECURITY=insecure
#' @param openBrowser this indicates wether the r session can open a browser to user can access. default is true. if set to false a URL and a code is written to the console. This can be used to log in form a different PC.
#' it can be set to false also by the environment variable IMPROVER_HEADLESS_OAUTH=T
#' @param withCodeVerifier if the oauth provider uses pkca code challenge verification
#' @references ics1081
#' @export

improveOAuth <- function(repo,shortEntityId="/",logLevel="INFO",secure=T,openBrowser=T,withCodeVerifier=T) {


  if (Sys.getenv("IMPROVER_HEADLESS_OAUTH")!="" ||
      (Sys.getenv("IMPROVER_TEST_REPLAY")=="T" && (!isCapturing()))) {
    openBrowser=F
  }

  secureFlag <- Sys.getenv("IMPROVER_SECURITY")
  if (!is.null(secureFlag) && secureFlag=="insecure") {
    secure<-F
  }
  if (!secure) {
    httr::set_config(httr::config(ssl_verifypeer = 0L))
    httr::set_config(httr::config(ssl_verifyhost = 0L))
  }

  log_info("authenticating with oauth against repository",repo)
  if (!is.character(repo)) {
    log_warn("repo needs to be a valid URL in the form https://<url>:<port>/<repositoryPath>")
    return(F)
  }
  authenticationProvider <- NULL
  tryCatch(
    {
      authenticationProvider <- getAuthenticationProvider(repo) %>%
        startOAuth(withCodeVerifier) %>%
        showOAuth(openBrowser) %>%
        pollToken()
    },error=function(e) {
      log_error("Error authenticating via oauth")
      log_error(e)
      return(F)
    }
  )

  print(authenticationProvider$id_token)
  user <- jose::jwt_split(authenticationProvider$id_token)$payload$preferred_username


  Sys.setenv(IMPROVER_REPO_URL=repo)
  Sys.setenv(IMPROVER_USER=user)
  Sys.setenv(IMPROVER_TOKEN=authenticationProvider$access_token)
  Sys.setenv(IMPROVER_STEP=shortEntityId)


  Sys.setenv(IMPROVER_TOKEN_EXPIRATION=authenticationProvider$expires_in)
  Sys.setenv(IMPROVER_REFRESH_TOKEN=authenticationProvider$refresh_token)
  Sys.setenv(IMPROVER_LAST_ACCESS=as.numeric(Sys.time()))


  improveConnect(logLevel,secure)

  setUser(user)


}

improveRevokeOAuth <- function() {
  repo <- substr(conf()$repoUrl,0,nchar(conf()$repoUrl)-8)

  authenticationProvider <- getAuthenticationProvider(repo)

  urlParams <- list()
  urlParams$token_type_hint <-"access_token"
  urlParams$client_id <- authenticationProvider$clientId
  urlParams$token<-Sys.getenv("IMPROVER_TOKEN")

  revokeResult <- httr::POST(authenticationProvider$deviceAuthUri,encode = "form",body=urlParams)
  if (revokeResult$status_code!=200) {
    stop("error getting device code")
  }


  clearConnectionData()


}




# repo <- "http://envhost2.hc.scintecodev.internal:18118/repository"


getAuthenticationProvider <- function(repo) {
  authenticationProviderApi <- file.path(
    repo,
    "api/v1/authentication/provider"
  )
  authenticationProviderResult <- httr::GET(authenticationProviderApi)
  if (authenticationProviderResult$status_code!=200) {
    stop(paste("no authentication provider found at ",authenticationProviderApi))
  }
  authenticationProvider <- httr::content(authenticationProviderResult)
  cacheEnv$authenticationProvider <- authenticationProvider
  return(authenticationProvider)
}

createCodeVerifier <- function() {
  #if we are capturing for replay, we have to have the same code verifier everytime, otherwise the replay wont work
  if (isCapturing() || Sys.getenv("IMPROVER_TEST_REPLAY")=="T") {
    return(
      "WUsHGZRCV9NGaRfp9RlaMl4NQvLx8TNtrUj5crJYXH7wTJcaxt4ykP7AAJ41kVtICGfmzdUacdACgQ6y5OlTz6bt9CX1Hc5oyb6F6K5ovlPAQ-GuRBdZlOGw4vsoXSas"
    )
  }

  return(
    return(jose::base64url_encode(openssl::aes_keygen(96)))
  )
}


hashToRaw <- function(hash_hex) {
  hash_raw <- as.raw(sapply(seq(1, nchar(hash_hex), by = 2), function(i) {
    strtoi(substr(hash_hex, i, i+1), base = 16L)
  }))
  return(hash_raw)
}

createCodeChallenge <- function(verifier) {

    # Compute the SHA256 hash as a hex string
    hash_hex <- openssl::sha256(verifier)

    # Convert the hexadecimal string to raw bytes
    hash_raw <- hashToRaw(hash_hex)

    # Encode the raw hash using base64url encoding
    return(jose::base64url_encode(hash_raw))

}

startOAuth <- function(authenticationProvider,withCodeVerifier) {
  urlParams <- list()
  urlParams$response_type <-"code"
  urlParams$client_id <- authenticationProvider$clientId
  urlParams$scope <- "openid profile email"
  if (withCodeVerifier) {
    if (is.null(cacheEnv$codeVerifier)) {
      cacheEnv$codeVerifier<- createCodeVerifier()
    }
    authenticationProvider$codeVerifier<-cacheEnv$codeVerifier
    authenticationProvider$codeChallengeMethod <- "S256"
    authenticationProvider$codeChallenge <- createCodeChallenge(authenticationProvider$codeVerifier)

    urlParams$code_challenge <- authenticationProvider$codeChallenge
    urlParams$code_challenge_method <- authenticationProvider$codeChallengeMethod
  }
  deviceCodeResult <- httr::POST(authenticationProvider$deviceAuthUri,encode = "form",body=urlParams)
  if (deviceCodeResult$status_code!=200) {
    stop("error getting device code")
  }
  deviceCodeContent <- httr::content(deviceCodeResult)
  authenticationProvider <- c(deviceCodeContent,authenticationProvider)
  return(authenticationProvider)
}


showOAuth <- function(authenticationProvider,openBrowser) {
  if (openBrowser) {
    utils::browseURL(authenticationProvider$verification_uri_complete)
  } else {
    print(paste0("visit this URL: ",authenticationProvider$verification_uri))
    print(paste0("your usercode is: ",authenticationProvider$user_code))
  }
  return(authenticationProvider)
}




pollToken <- function (authenticationProvider) {



  while (authenticationProvider$expires_in>0) {
    startTime<-as.numeric(Sys.time())
    if (isCapturing()) {
      print("login within 30 seconds")
      Sys.sleep(30)
    }
    pollResult <- hasAuthenticated(authenticationProvider)
    if (pollResult$status_code==200) {
      pollContent <- httr::content(pollResult)
      return(c(authenticationProvider,pollContent))
    }

    Sys.sleep(authenticationProvider$interval)
    stopTime<-as.numeric(Sys.time())
    duration <- stopTime-startTime
    authenticationProvider$expires_in <- authenticationProvider$expires_in-duration
  }
  stop("authentication timed out")

}

hasAuthenticated <- function(authenticationProvider) {
  urlParams <- list()
  urlParams$grant_type <-"urn:ietf:params:oauth:grant-type:device_code"
  urlParams$client_id <- authenticationProvider$clientId
  urlParams$device_code <- authenticationProvider$device_code
  urlParams$scope <- "openid profile email"
  if ("codeVerifier" %in% names(authenticationProvider)) {
    urlParams$code_verifier <- authenticationProvider$codeVerifier
  }
  pollResult <- httr::POST(authenticationProvider$tokenUri,encode = "form",body=urlParams)
  return(pollResult)
}

