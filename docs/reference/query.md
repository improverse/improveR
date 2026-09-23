# Search Resources Using Elasticsearch Query

Executes a search query against the improve platform's Elasticsearch
index to find resources matching the specified criteria. Uses the same
query syntax as the improve web UI search interface, supporting
field-based searches, wildcards, and logical operators.

## Usage

``` r
query(query, fetchSize = 100, fetchOffset = 0)
```

## Arguments

- query:

  Character. Search query using improve's query grammar. Examples:

  - `"name='analysis'"` - exact name match

  - `"name='*analysis*'"` - name contains "analysis"

  - `"nodeType='Step' AND runStatus='FINISHED'"` - combined criteria

  - `"description='*population*'"` - search in description field

- fetchSize:

  Numeric. Maximum number of results to return. Defaults to 100. Use for
  pagination or limiting large result sets.

- fetchOffset:

  Numeric. Number of results to skip before returning matches. Defaults
  to 0. Use with `fetchSize` to implement pagination.

## Value

A data frame containing the matched resources with their metadata, or
`NULL` if no resources match the query or if the query contains errors.
The data frame includes standard resource fields like `resourceId`,
`entityId`, `name`, `path`, `nodeType`, and any additional fields
returned by the search (depending on the query).

## Details

The function submits the query to the improve platform's Elasticsearch
backend, which indexes resource metadata for fast searching. Search
results include the `resourceId` for each match, which is then used to
load full resource details.

Query syntax supports:

- Field searches: `fieldName='value'`

- Wildcards: `fieldName='*partial*'`

- Logical operators: `AND`, `OR`

- Date fields: use ISO format for date comparisons

- Path-based searches: `path='/Some/Folder/*'`

If the query contains errors, a warning is logged with the error message
from the server, and `NULL` is returned.

For searches within a specific folder hierarchy, use
[`queryFolder`](https://improverse.github.io/improveR/reference/queryFolder.md)
which automatically restricts results to a folder and its descendants.

## References

ics1143

## See also

[`queryFolder`](https://improverse.github.io/improveR/reference/queryFolder.md)
for folder-scoped searches,
[`loadResource`](https://improverse.github.io/improveR/reference/loadResource.md)
to load resources by known identifiers

## Examples

``` r
if (FALSE) { # \dontrun{
# Find all finished steps
query("nodeType='Step' AND runStatus='FINISHED'")

# Search for resources by name pattern
query("name='*baseline*'")

# Paginate through large result sets
page1 <- query("nodeType='File'", fetchSize = 50, fetchOffset = 0)
page2 <- query("nodeType='File'", fetchSize = 50, fetchOffset = 50)

# Find resources in a specific path
query("path='/Modeling/PopPK/*'")
} # }
```
