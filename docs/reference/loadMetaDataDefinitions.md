# Loads All Meta Data Definitions For A Scope, The Default Scope Is "Improve Client"

Loads All Meta Data Definitions For A Scope, The Default Scope Is
"Improve Client"

## Usage

``` r
loadMetaDataDefinitions(scope = "Improve Client")
```

## Arguments

- scope:

  the metadata scope

## Value

A data frame of every metadata definition in the scope, one row each, as
the server returns them. Cached: the second call for the same scope does
not reach the server.

## References

ics1137
