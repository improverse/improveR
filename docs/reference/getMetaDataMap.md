# Loads the Metadata of a Given Resource and Writes It as String Values Into a List

Loads the Metadata of a Given Resource and Writes It as String Values
Into a List

## Usage

``` r
getMetaDataMap(ident, from = pwd())
```

## Arguments

- ident:

  path, resource or entity ID of the picture

- from:

  used for relative paths, by default pwd is used, which is initiated
  with the step that started improveR

## Value

A named list, one entry per metadata descriptor, holding the value as
text - dates as POSIX values. `NULL` when the resource has no metadata,
and an empty list when it has entries but none of them carries a value.
