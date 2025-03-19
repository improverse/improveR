

# Renews the access token using the refresh token stored in the session storage.
# Retrieves and parses stored OAuth data and sends a POST request to the token endpoint.
# Updates the session storage with the new token data on successful renewal.
# Throws an error if the stored data is missing or if the renewal request fails.
#
# @return {Promise<Object>} A promise that resolves to the renewed token data object containing the access token and optionally the refresh token.
# @throws {Error} If the stored OAuth data is missing, the token renewal fails, or if the response is not ok.

renewAccessToken <- function () {
    refrToken = Sys.getenv("IMPROVER_REFRESH_TOKEN")
    if (refrToken=="") {
      stop('Could not find stored refresh token')
    }
    storedData = decodeRefreshToken(refrToken)
    authProvider = cacheEnv$authenticationProvider
    #currentTokenString = Sys.getenv("IMPROVER_TOKEN")
    #currentToken = jose::jwt_split(currentTokenString)

    urlParams = list(
      grant_type= "refresh_token",
      client_id= authProvider$clientId,
      refresh_token= refrToken
    )

    refreshCodeResult <- httr::POST(authProvider$tokenUri,encode = "form",body=urlParams)
    if (refreshCodeResult$status_code!=200) {
      stop("error getting token refreshed")
    }
    refreshContent <- httr::content(refreshCodeResult)

    Sys.setenv(IMPROVER_TOKEN=refreshContent$access_token)


    Sys.setenv(IMPROVER_TOKEN_EXPIRATION=refreshContent$expires_in)
    Sys.setenv(IMPROVER_REFRESH_TOKEN=refreshContent$refresh_token)
    Sys.setenv(IMPROVER_LAST_ACCESS=as.numeric(Sys.time()))

  }




decodeRefreshToken <- function (refreshToken) {
  tryCatch( {
    decodedRefreshToken = jose::jwt_split(refreshToken)$payload

    # Validierung
    if (is.null(decodedRefreshToken$exp)) {
      stop('invalid refresh token');
    }

    # Prüfe Token-Ablauf
    currentTime = floor(as.numeric(Sys.time()) / 1000)
    if (decodedRefreshToken$exp < currentTime) {
      stop('Refresh Token expired')
    }

    return (
      list(
        tokenDetails= decodedRefreshToken,
        isExpired= decodedRefreshToken$exp < currentTime,
        timeUntilExpiration= decodedRefreshToken$exp - currentTime
      )
    )
  },error= function (error) {
    log_error('Error decoding Refresh Token:', error);
    stop(error)
  })
}

