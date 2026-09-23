# Loads One Meta Data Definitions By Name For A Scope, The Default Scope Is "Improve Client"

Loads One Meta Data Definitions By Name For A Scope, The Default Scope
Is "Improve Client"

## Usage

``` r
loadMetaDataDefinition(name, scope = "Improve Client")
```

## Arguments

- name:

  name of the meta data definition

- scope:

  the metadata scope

## Value

The single row of
[`loadMetaDataDefinitions()`](https://improverse.github.io/improveR/reference/loadMetaDataDefinitions.md)
whose `name` matches, as a data frame. A name that does not exist in the
scope yields a data frame with zero rows, not `NULL`.

## References

ics1137
