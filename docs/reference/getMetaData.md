# Gets a Metadata Data Frame

Gets a Metadata Data Frame

## Usage

``` r
getMetaData(ident, from = pwd())
```

## Arguments

- ident:

  path, resource or entity ID of the picture

- from:

  used for relative paths, by default pwd is used, which is initiated
  with the step that started improveR

## Value

A data frame with one row per metadata entry and a fixed column order,
starting with `descriptorName`, `value` and `descriptorType`. `value`
carries the entry as text, taken from the column that matches its type -
`textValue`, `lovText`, `numValue`, or `dateValue` converted to a POSIX
date. `NULL` when the resource has no metadata.

## References

ics1141
