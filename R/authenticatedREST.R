REST_FUNCTIONS <- list(POST="httr::POST",
                       GET="httr::GET",
                       PUT="httr::PUT",
                       DELETE="httr::DELETE")

restEnv <- new.env()
restEnv$lastRestError <- NULL

timing <- function(name) {
  #log_info(paste(
  #  name,
  #  Sys.time()
  #))
}

#' Last REST Error
#'
#' @description Returns details about the last failed REST call, or NULL if the last call succeeded.
#' Since improveR is single-threaded, this is safe to use after any REST operation.
#' @returns A list with \code{status_code}, \code{url}, \code{method}, \code{message},
#' and \code{timestamp}, or \code{NULL} if the last call was successful.
#' @export
lastRestError <- function() {
  return(restEnv$lastRestError)
}

#' @noRd
setLastRestError <- function(status_code, url, method, message) {
  restEnv$lastRestError <- list(
    status_code = status_code,
    url = url,
    method = method,
    message = message,
    timestamp = Sys.time()
  )
}

#' @noRd
clearLastRestError <- function() {
  restEnv$lastRestError <- NULL
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
#'
#' In this example the following URL would be constructed:
#'   baseURL from improveConnect /resources/1B1D3B817F424BA594893A5013DBFEEA?isResourceVersion=true
#'
#' On success (HTTP 2xx), returns the httr response object.
#' On failure, returns NULL and stores error details accessible via \code{lastRestError()}.
#' @seealso [improveConnect()], [lastRestError()]
#' @references ics1082
#' @keywords internal
#' @noRd
authenticatedREST <- function(url,urlParams=list(),queryParams=list(),data="",restType="GET",contentType="application/json",encode="json") {
  timing(url)
  if (is.null(encode)) {
    encode <- c("multipart","form", "json", "raw")
  }

  if (startsWith(url,"/")) {
    url <- substr(url,2,nchar(url))
  }
  if (!(restType) %in% names(REST_FUNCTIONS)) {
    log_error("trying to use an undefined REST method:", restType)
    setLastRestError(NA, url, restType, "undefined REST method")
    return(NULL)
  }
  restFunction <- REST_FUNCTIONS[restType][[1]]
  #fix for httptest to work
  restFunction <- eval(parse(text=restFunction))

  url <- replacePlaceHoldersinURL(url,urlParams)
  url <- appendQueryParams(url,queryParams)
  fullUrl <- paste0(conf()$repoUrl,url)
  log_debug(restType, "connecting to", fullUrl)

  # Display REST calls if environment variable is set
  if (Sys.getenv("IMPROVER_DISPLAY_REST_CALLS", "") != "") {
    cat(paste0("REST Call: ", restType, " ", fullUrl, "\n"))
    if (!identical(data, "")) {
      # Check if data is binary (raw type or contains file upload)
      if (is.raw(data) || (is.list(data) && any(sapply(data, function(x) inherits(x, "form_file"))))) {
        cat("Data: <binaryBlob>\n")
      } else {
        cat("Data: ", jsonlite::toJSON(data, auto_unbox = TRUE, pretty = TRUE), "\n")
      }
    }
  }

  result <- NULL
  if (!is.na(conf()$reqToken) && !is.null(conf()$reqToken)) {
    log_debug("TokenAuth")
    if (!grepl("refreshToken",fullUrl)) {
      log_debug("Check refresh Token")
      refreshToken()
    } else {
      log_debug("Skipping refresh for token endpoint")
    }

    result <- restFunction(fullUrl, body = data,
                           httr::add_headers('Authorization' = paste0("Bearer ",conf()$reqToken)),
                                             encode=encode,
                                             'Content-Type' = contentType)


  } else {
    log_debug("User Auth")
    result <- restFunction(fullUrl, body = data,
                           httr::authenticate(conf()$user,conf()$password),
                           encode=encode,
                           'Content-Type' = contentType)
  }
  if ((result$status_code>=200 && result$status_code<300)) {
    log_debug(result$status_code, fullUrl)
    clearLastRestError()
    timing("done")
    return(result)
  } else if (result$status_code==401){
    log_error("Invalid authentication credentials (token or username/password)")
    setLastRestError(result$status_code, fullUrl, restType, "Invalid authentication credentials")
    return(NULL)
  } else if (result$status_code == 417) {
    log_error("Resource could not be run")
    setLastRestError(result$status_code, fullUrl, restType, "Resource could not be run")
    return(NULL)
  } else {
    log_warn(result$status_code, "error when connecting to", fullUrl)
    setLastRestError(result$status_code, fullUrl, restType,
                     paste0("HTTP ", result$status_code, " from ", fullUrl))
    return(NULL)
  }
}

replacePlaceHoldersinURL <- function(url,urlParams) {
  if (length(urlParams)>0) {
    for (i in 1:length(urlParams)) {
      urlParamName <- paste0("{",names(urlParams[i]),"}")
      urlParamValue <- as.character(urlParams[i][1])
      url<-gsub(urlParamName,urlParamValue,url,fixed=TRUE)
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
