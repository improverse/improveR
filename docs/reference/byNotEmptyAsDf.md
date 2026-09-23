# Apply Function Row-Wise and Merge Results to Data Frame

Applies a function to each row of a data frame and automatically merges
the results back into a single data frame. This is a convenience wrapper
around
[`byNotEmpty`](https://improverse.github.io/improveR/reference/byNotEmpty.md)
with automatic result consolidation, handling NULL or empty inputs
gracefully.

## Usage

``` r
byNotEmptyAsDf(df, func)
```

## Arguments

- df:

  Data frame to iterate over. If `NULL`, not a data frame, or has zero
  rows, the function returns `NULL` without attempting iteration.

- func:

  Function to apply to each row. The function receives one argument: a
  single-row data frame representing the current row. Should return a
  single-row data frame, a named list, or another structure that can be
  coerced to a data frame row.

## Value

A data frame containing the merged results from all row applications, or
`NULL` if the input data frame is empty, `NULL`, or invalid. Rows from
individual function calls are combined using `mergeDataframeList()`.

## Details

This function combines
[`byNotEmpty`](https://improverse.github.io/improveR/reference/byNotEmpty.md)
with automatic data frame consolidation, making it ideal for
transforming data frames row-by-row when each transformation produces a
data frame row as output.

Common use cases include:

- Enriching each row with additional data from API calls

- Expanding rows that contain nested data structures

- Computing row-wise aggregations that return structured results

The function internally uses `mergeDataframeList()` to intelligently
combine heterogeneous row results, handling varying column sets
gracefully.

## See also

[`byNotEmpty`](https://improverse.github.io/improveR/reference/byNotEmpty.md)
for row-wise iteration without automatic merging,
[`by`](https://rdrr.io/r/base/by.html) for the underlying iteration
mechanism

## Examples

``` r
if (FALSE) { # \dontrun{
# Transform each resource row
steps <- loadChildSteps("/Workflow")$data[[1]]
enriched <- byNotEmptyAsDf(steps, function(row) {
  data.frame(
    stepName = row$name,
    status = row$runStatus,
    isFinished = row$runStatus == "FINISHED"
  )
})

# Expand nested structures
byNotEmptyAsDf(parentData, function(row) {
  # Load children for this parent
  children <- loadChildResources(row$resourceId)$data[[1]]
  # Return summary
  data.frame(parent = row$name, childCount = nrow(children))
})
} # }
```
