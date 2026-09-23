# Improve OAuth

improveOAuth is used to connect to the repository via oauth.

## Usage

``` r
improveOAuth(
  repo,
  shortEntityId = "/",
  logLevel = "INFO",
  secure = T,
  openBrowser = T,
  withCodeVerifier = T
)
```

## Arguments

- repo:

  the repository URL in this form https://:/ the api part (/api/v1) is
  added automatically

- shortEntityId:

  one valid short entity ID must be provided, this is used as
  pathWorkingDirectory

- logLevel:

  possible LogLevels: DEBUG, INFO, WARN, ERROR

- secure:

  if TRUE the certificates are checked. Default is TRUE, it can be set
  to false also by the environment variable IMPROVER_SECURITY=insecure

- openBrowser:

  this indicates whether the R session can open a browser for user
  access. Default is TRUE. If set to FALSE, a complete verification URL
  is logged to the console for manual access. This enables headless
  authentication for CI/CD environments or remote sessions. Can be
  controlled by environment variable IMPROVER_HEADLESS_OAUTH (any
  non-empty value).

- withCodeVerifier:

  if the oauth provider uses pkca code challenge verification

## Value

No meaningful value - called for its side effects: the token, refresh
token, expiry, repository URL, user and step are written to the
environment, and
[`improveConnect()`](https://improverse.github.io/improveR/reference/improveConnect.md)
is called with them. `FALSE` when `repo` is not a character string.
Stops with `Error authenticating via oauth` when the flow fails.

## References

ics1081
