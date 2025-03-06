
#' improveLogin 
#' 
#' @description improveLogin can be used for test, development, or interactive uses of improveR.
#' It uses the username and password to retrieve a token.
#' @param repo the repository URL in this form https://<url>:<port>/<repositoryPath> the api part (/api/v1) is added automatically
#' @param user the username as character
#' @param password the plain text password as character, is not logged or stored
#' @param shortEntityId one valid short entity ID must be provided, this is used as pathWorkingDirectory
#' @param logLevel possible LogLevels: DEBUG, INFO, WARN, ERROR
#' @param secure if TRUE the certificates are checked
#' @references ics1081
#' @export

improveLogin <- function(repo,user,password,shortEntityId,logLevel="INFO",secure=T) {

  secureFlag <- Sys.getenv("IMPROVER_SECURITY")
  if (!is.null(secureFlag) && secureFlag=="insecure") {
    secure<-F
  }
  if (!secure) {
    httr::set_config(httr::config(ssl_verifypeer = 0L))
    httr::set_config(httr::config(ssl_verifyhost = 0L))
  }

  log_info("retrieving token for user",user,"from repository",repo)
  if (!is.character(repo)) {
    log_warn("repo needs to be a valid URL in the form https://<url>:<port>/<repositoryPath>")
    return(F)
  }
  if (!is.character(user)) {
    log_warn("user needs to be of type character")
    return(F)
  }
  if (!is.character(password)) {
    log_warn("password needs to be of type character")
    return(F)
  }
  result <- httr::POST(paste0(repo,"/api/v1/authentication/"),
                         httr::authenticate(user,password),
                         'Content-Type' = "application/json")
  if (result$status_code==200) {
    log_info("token succesfully acquired")
    content <- httr::content(result)
    contentString <- rawToChar(content)

    Sys.setenv(IMPROVER_REPO_URL=repo)
    #Sys.setenv(IMPROVER_TOKEN=contentData$access_token)
    Sys.setenv(IMPROVER_USER=user)
    Sys.setenv(IMPROVER_TOKEN=contentString)
    Sys.setenv(IMPROVER_STEP=shortEntityId)

    tryCatch(
      {
        contentData <- jsonlite::parse_json(contentString)
        Sys.setenv(IMPROVER_TOKEN_EXPIRATION=contentData$expires_in)
        Sys.setenv(IMPROVER_REFRESH_TOKEN=contentData$refresh_token)
        Sys.setenv(IMPROVER_LAST_ACCESS=as.numeric(Sys.time()))
      }, error=function(e) {
        log_info("no refresh token")
        Sys.setenv(IMPROVER_TOKEN_EXPIRATION=as.numeric(Sys.time())+300)
        Sys.setenv(IMPROVER_REFRESH_TOKEN=contentString)
        Sys.setenv(IMPROVER_LAST_ACCESS=as.numeric(Sys.time()))
      }
    )

    improveConnect(logLevel,secure)

    setUser(user)
    return(T)
  } else if (result$status_code==200) {   #QUESTION: else if (result$status_code=200) will never be triggered because above same condition
    log_error("not allowed to retrieve token")
    stop("not allowed to retrieve token")
  } else if (result$status_code==404){
    log_error(repo,"url cannot be found")
    stop("url cannot be found")
  } else {
    log_error("general error")
    stop("general error")
  }
}

#' improveReLogin
#' @description improveReLogin can be used if a token could not be refreshed for test, development, or interactive uses 
#' of improveR. The function uses the password to retrieve a token. The user has to have logged in before with improveLogin.
#' @param password The plain password as a character string. The password is not logged or stored.
#' @references ics1208
#' @export
improveReLogin <- function(password) {
  user <- conf()$user
  result <- httr::POST(paste0(conf()$repoUrl,"authentication/"),
                       httr::authenticate(user,password),
                       'Content-Type' = "application/json")
  if (result$status_code==200) {
    log_info("token succesfully acquired")
    content <- httr::content(result)
    contentString <- rawToChar(content)
    Sys.setenv(IMPROVER_TOKEN=contentString)

    tryCatch(
      {
        contentData <- jsonlite::parse_json(contentString)
        Sys.setenv(IMPROVER_TOKEN_EXPIRATION=contentData$expires_in)
        Sys.setenv(IMPROVER_REFRESH_TOKEN=contentData$refresh_token)
        Sys.setenv(IMPROVER_LAST_ACCESS=as.numeric(Sys.time()))
      }, error=function(e) {
        log_info("no refresh token")
        Sys.setenv(IMPROVER_TOKEN_EXPIRATION=as.numeric(Sys.time())+300)
        Sys.setenv(IMPROVER_REFRESH_TOKEN=contentString)
        Sys.setenv(IMPROVER_LAST_ACCESS=as.numeric(Sys.time()))
      }
    )
    return(T)
  } else if (result$status_code==200) {
    log_error("not allowed to retrieve token")
    stop("not allowed to retrieve token")
  } else if (result$status_code==404){
    log_error(conf()$repoUrl,"url cannot be found")
    stop("url cannot be found")
  } else {
    log_error("general error")
    stop("general error")
  }
}


#' refreshToken
#' @description refreshToken requests a new token to access the system. Just used in combination with improveLogin. 
#' Run tokens do not need to be refreshed.
#' @param alwaysRefresh refresh no matter how much time has elapsed
#' @references ics1208
#' @seealso [improveLogin()]
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
        log_info("refreshing token")
        Sys.setenv(IMPROVER_LAST_ACCESS=as.numeric(Sys.time()))
        tryCatch(
          {
            result <- authenticatedREST("/authentication/refreshToken")
            if (!is.null(result)) {

              refreshContent <- httr::content(result)
              resultString <- jsonlite::toJSON(refreshContent)
              resultString  <- stringr::str_replace_all(resultString,"\\[","")
              resultString <- stringr::str_replace_all(resultString,"\\]","")
              Sys.setenv(IMPROVER_TOKEN=resultString)
              cacheEnv$conf$reqToken <- resultString
              log_debug(resultString)
            } else {
              log_warn("error refreshing token")
            }
          }, error=function(e) {
            result <- httr::GET(paste0(Sys.getenv("IMPROVER_REPO_URL"),"/api/v1/authentication/refreshToken"), body = list(),
                                httr::add_headers('Authorization' = paste0("Bearer ",cacheEnv$conf$reqToken),
                                                  'Content-Type' = "")
            )
            if (result$status_code==200) {
              resultString <- rawToChar(result$content)
              Sys.setenv(IMPROVER_TOKEN=resultString)
              cacheEnv$conf$reqToken <- resultString
              log_debug(resultString)
            } else {
              log_warn("error refreshing token")
            }
          }
        )

      }
    }
  }
}
