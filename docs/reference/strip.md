# Strip

strip removes the information data frame around subentities like meta
data, history, children, or audittrail.

## Usage

``` r
strip(data)
```

## Arguments

- data:

  a data frame with a nested data frame in \$data

## Value

The nested data frame in `$data` - the sub-entities themselves, without
the one-row wrapper the load functions put around them. Returns `NULL`
when `data` carries no `data` column.
