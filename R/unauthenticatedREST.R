#' Unauthenticated REST
#'
#' @description unauthenticatedREST performs REST calls without authentication headers.
#' This is used for OAuth flows and other endpoints that don't require authentication.
#' The function respects the IMPROVER_DISPLAY_REST_CALLS environment variable to display REST calls.
#' @param url The full URL to call (including protocol and domain)
#' @param restType character, default is GET, possible values are POST, GET, PUT and DELETE
#' @param body The data to send in the request body
#' @param encode How to encode the body (e.g., "form", "json", "multipart", "raw")
#' @param ... Additional arguments passed to the httr function
#' @return The httr response object
#' @export
unauthenticatedREST <- function(url, restType = "GET", body = NULL, encode = "json", ...) {
  
  # Display REST calls if environment variable is set
  if (Sys.getenv("IMPROVER_DISPLAY_REST_CALLS", "") != "") {
    cat(paste0("REST Call: ", restType, " ", url, "\n"))
    if (!is.null(body)) {
      # Check if data is binary
      if (is.raw(body)) {
        cat("Data: <binaryBlob>\n")
      } else {
        cat("Data: ", jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE), "\n")
      }
    }
  }
  
  # Select the appropriate httr function
  rest_function <- switch(restType,
    "GET" = httr::GET,
    "POST" = httr::POST,
    "PUT" = httr::PUT,
    "DELETE" = httr::DELETE,
    stop(paste("Unsupported REST type:", restType))
  )
  
  # Make the REST call
  if (!is.null(body)) {
    result <- rest_function(url, body = body, encode = encode, ...)
  } else {
    result <- rest_function(url, ...)
  }
  
  return(result)
}
