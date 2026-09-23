# Update access token in current session

Updates the access token in both environment variables and cached
configuration

## Usage

``` r
updateAccessToken(
  token,
  expiration = NULL,
  refreshToken = NULL,
  lastAccess = NULL
)
```

## Arguments

- token:

  Character. The new access token

- expiration:

  Character or numeric. Token expiration time

- refreshToken:

  Character. Optional refresh token

- lastAccess:

  Character or numeric. Optional last access time

## Value

`TRUE`, invisibly. Stops with an error if `token` is `NULL` or empty.
`IMPROVER_LAST_ACCESS` is always set, to the current time when
`lastAccess` is not given.
