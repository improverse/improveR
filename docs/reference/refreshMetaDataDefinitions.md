# Refresh Meta Data Definitions

Refresh Meta Data Definitions

## Usage

``` r
refreshMetaDataDefinitions(scope = "Improve Client")

updateMetaDataDefinitions(...)
```

## Arguments

- scope:

  the metadata scope

- ...:

  For backwards compatibility with the deprecated `update*` alias; not
  used by `refresh*` itself.

## Value

The freshly read metadata definitions for the scope, in the same shape
as
[`loadMetaDataDefinitions()`](https://improverse.github.io/improveR/reference/loadMetaDataDefinitions.md).
The definitions are repository-wide configuration, so this affects every
later call in the session, not only the caller's.

## References

ics1137
