# Apply token data to session

Applies a complete token data structure to the current session

## Usage

``` r
applyTokenData(tokenData)
```

## Arguments

- tokenData:

  List containing token information with fields: IMPROVER_TOKEN,
  IMPROVER_TOKEN_EXPIRATION, IMPROVER_REFRESH_TOKEN,
  IMPROVER_LAST_ACCESS, IMPROVER_REPO_URL, IMPROVER_USER

## Value

`TRUE`, invisibly. Only the fields present in `tokenData` are applied;
the others keep their current value. Stops with an error if `tokenData`
is not a list.
