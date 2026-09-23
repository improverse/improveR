# Unauthenticated REST

unauthenticatedREST performs REST calls without authentication headers.
This is used for OAuth flows and other endpoints that don't require
authentication. The function respects the IMPROVER_DISPLAY_REST_CALLS
environment variable to display REST calls.

## Usage

``` r
unauthenticatedREST(url, restType = "GET", body = NULL, encode = "json", ...)
```

## Arguments

- url:

  The full URL to call (including protocol and domain)

- restType:

  character, default is GET, possible values are POST, GET, PUT and
  DELETE

- body:

  The data to send in the request body

- encode:

  How to encode the body (e.g., "form", "json", "multipart", "raw")

- ...:

  Additional arguments passed to the httr function

## Value

The httr response object
