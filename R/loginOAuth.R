#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' improveOAuth
#'
#' @description improveOAuth is used to connect to the repository via oauth.
#' @param repo the repository URL in this form https://<url>:<port>/<repositoryPath> the api part (/api/v1) is added automatically
#' @param shortEntityId one valid short entity ID must be provided, this is used as pathWorkingDirectory
#' @param logLevel possible LogLevels: DEBUG, INFO, WARN, ERROR
#' @param secure if TRUE the certificates are checked
#' @param openBrowser this indicates wether the r session can open a browser to user can access. default is true. if set to false a URL and a code is written to the console. This can be used to log in form a different PC.
#' @param withCodeVerifier if the oauth provider uses pkca code challenge verification
#' @references ics1081
#' @export

improveOAuth <- function(repo,shortEntityId,logLevel="INFO",secure=T,openBrowser=T,withCodeVerifier=F) {

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





#repo <- "http://envhost2.hc.scintecodev.internal:18118/repository"


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
  return(authenticationProvider)
}

startOAuth <- function(authenticationProvider,withCodeVerifier) {
  urlParams <- list()
  urlParams$response_type <-"code"
  urlParams$client_id <- authenticationProvider$clientId
  urlParams$scope <- "openid profile email"
  if (withCodeVerifier) {
    authenticationProvider$codeVerifier <- openssl::base64_encode(openssl::aes_keygen(96))
    authenticationProvider$codeChallengeMethod <- "S256"
    authenticationProvider$codeChallenge <- openssl::base64_encode(openssl::sha256(authenticationProvider$codeVerifier)) # @HACKLM # QUESTION R CMD checks no binding for global variable codeVerifier; where does it come from? should it be authenticationProvider$codeVerifier?

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
    print("visit this URL: ",authenticationProvider$verification_uri )
  }
  return(authenticationProvider)
}




pollToken <- function (authenticationProvider) {



  while (authenticationProvider$expires_in>0) {
    startTime<-as.numeric(Sys.time())

    urlParams <- list()
    urlParams$grant_type <-"urn:ietf:params:oauth:grant-type:device_code"
    urlParams$client_id <- authenticationProvider$clientId
    urlParams$device_code <- authenticationProvider$device_code
    urlParams$scope <- "openid profile email"
    if ("codeVerifier" %in% names(authenticationProvider)) {
      urlParams$code_verifier <- authenticationProvider$codeVerifier
    }
    pollResult <- httr::POST(authenticationProvider$tokenUri,encode = "form",body=urlParams)
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

