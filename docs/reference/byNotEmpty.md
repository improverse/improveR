# Apply Function Row-Wise With Null Protection

Applies a function to each row of a data frame, with automatic handling
of NULL or empty inputs. This is a safer alternative to
[`base::by`](https://rdrr.io/r/base/by.html) that returns `NULL` instead
of throwing errors when given invalid or empty data frames.

## Usage

``` r
byNotEmpty(df, func)
```

## Arguments

- df:

  Data frame to iterate over. If `NULL`, not a data frame, or has zero
  rows, the function returns `NULL` without attempting iteration.

- func:

  Function to apply to each row. The function receives one argument: a
  single-row data frame representing the current row. Should return any
  value.

## Value

A `by` object containing the results from applying `func` to each row,
or `NULL` if the input data frame is empty, `NULL`, or invalid. The `by`
object can be converted to a list using
[`as.list()`](https://rdrr.io/r/base/list.html) or further processed.

## Details

This function wraps [`base::by()`](https://rdrr.io/r/base/by.html) with
null-safety checks, making it suitable for use in pipelines where data
frames may be empty or NULL. It's commonly used throughout improveR for
iterating over resource lists, child resources, and query results where
the number of results may be zero.

The function is applied to each row independently, with rows identified
by `seq_len(nrow(df))`. If you need the results automatically converted
back to a data frame, use
[`byNotEmptyAsDf`](https://improverse.github.io/improveR/reference/byNotEmptyAsDf.md)
instead.

## See also

[`byNotEmptyAsDf`](https://improverse.github.io/improveR/reference/byNotEmptyAsDf.md)
for automatic conversion of results to data frame,
[`by`](https://rdrr.io/r/base/by.html) for the underlying iteration
mechanism

## Examples

``` r
if (FALSE) { # \dontrun{
# Safely iterate over resources
resources <- loadChildResources("/Data")$data[[1]]
byNotEmpty(resources, function(row) {
  cat("Processing:", row$name, "\n")
  # Returns NULL for empty result sets
})

# Extract specific fields from each row
result <- byNotEmpty(resources, function(row) {
  list(name = row$name, type = row$nodeType)
})
} # }
```
