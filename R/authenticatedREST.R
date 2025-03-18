REST_FUNCTIONS <- list(POST=httr::POST,
                       GET=httr::GET,
                       PUT=httr::PUT,
                       DELETE=httr::DELETE)

timing <- function(name) {
  #logging::loginfo(paste(
  #  name,
  #  Sys.time()
  #))
}

#' authenticatedREST
#'
#' @description authenticatedREST uses the connection information from improveConnect to create a REST call to the repository.
#' The values below are example values.
#' @param url in the format /resources/\{resourceId\}
#' @param urlParams in the format list(resourceId="1B1D3B817F424BA594893A5013DBFEEA")
#' @param queryParams in the format list(isResourceVersion="true")
#' @param data this block is written to the body, either list, binary or json
#' @param restType character, default is GET, possible values are POST,GET,PUT and DELETE
#' @param contentType is directly set to the header, default is application/json, used to specify the format for data
#' @param encode is directly set to the header, default is json, should be fine for most rest calls, for file uploads use NULL
#' @param ignoreFail is by default set to TRUE, if false, an error is thrown if the error code is not between 200 and 300
#'
#' In this example the following URL would be constructed:
#'   baseURL from improveConnect /resources/1B1D3B817F424BA594893A5013DBFEEA?isResourceVersion=true
#'
#' the unparsed result is returned from this call
#' @seealso [improveConnect()]
#' @references ics1082
#' @export
authenticatedREST <- function(url,urlParams=list(),queryParams=list(),data="",restType="GET",contentType="application/json",encode="json",ignoreFail=T) {
  timing(url)
  if (is.null(encode)) {
    encode <- c("multipart","form", "json", "raw")
  }

  if (startsWith(url,"/")) {
    url <- substr(url,2,nchar(url))
  }
  if (!(restType) %in% names(REST_FUNCTIONS)) {
    logging::logerror("trying to use an undefined REST method")
    logging::logerror(restType)
    return(NULL)
  }
  restFunction <- REST_FUNCTIONS[restType][[1]]
  url <- replacePlaceHoldersinURL(url,urlParams) 
  url <- appendQueryParams(url,queryParams)
  url <- paste0(conf()$repoUrl,url)
  logging::logdebug(paste0(restType," connecting to ",url))

  result <- NULL
  if (!is.na(conf()$reqToken) && !is.null(conf()$reqToken)) {
    logging::logdebug("TokenAuth")
    result <- restFunction(url, body = data,
                           httr::add_headers('Authorization' = paste0("Bearer ",conf()$reqToken)),
                                             encode=encode,
                                             'Content-Type' = contentType)
    if (!grepl("refreshToken",url)) {
      logging::logdebug("Check refresh Token")
      refreshToken()
    } else {
      logging::logdebug("do not refresh, as is token") # TODO wording ambiguous; e.g. "Do not refresh, valid token available."
    }

  } else {
    logging::logdebug("User Auth")
    result <- restFunction(url, body = data,
                           httr::authenticate(conf()$user,conf()$password),
                           encode=encode,
                           'Content-Type' = contentType)
  }
  if ((result$status_code>=200 && result$status_code<300)) {
    logging::logdebug(paste0(result$status_code," ",url))
    timing("done")
    return(result)
  } else if (result$status_code==401){
    return(NULL)
    log_error("Invalid or out of date token") # TODO wording ok, could be also invalid user/password combi; not only token; e.g. "Invalid authentication credentials (token, username/password)."
  } else {
    logging::logdebug(result)
    if (!ignoreFail) { #if ignoreFail is false, function stops
      logging::logerror(paste0(result$status_code," error when connecting to ",url))
      stop("error connecting to REST service")
    } else {
      logging::logdebug(paste0(result$status_code," error when connecting to ",url))
    }
    return(NULL)
  }
}

replacePlaceHoldersinURL <- function(url,urlParams) {
  if (length(urlParams)>0) {
    for (i in 1:length(urlParams)) {
      urlParamName <- paste0("{",names(urlParams[i]),"}")
      urlParamValue <- as.character(urlParams[i][1])
      url<-gsub(urlParamName,urlParamValue,url,fixed=T)
    }
  }
  return(url)
}

appendQueryParams <- function(url,queryParams) {
  queryParamFormatted <- lapply(names(queryParams),function(param) {
    paste0(param,"=",as.character(queryParams[param]))
  })
  queryParamFormattedString <- paste(queryParamFormatted,collapse = "&")
  queryParamFormattedString <- paste(url,queryParamFormattedString,sep="?")
}
