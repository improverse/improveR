# Search Resources Within a Folder Using Elasticsearch Query

Executes a search query limited to resources within a specific folder
and its descendants. This is a convenience wrapper around
[`query`](https://improverse.github.io/improveR/reference/query.md) that
automatically restricts the search scope to a folder hierarchy, using
the same query syntax as the improve web UI.

## Usage

``` r
queryFolder(queryString, fetchSize = 100, fetchOffset = 0, ident, from = pwd())
```

## Arguments

- queryString:

  Character. Search query using improve's query grammar. The query is
  automatically combined with a path restriction to limit results to the
  specified folder. Use the same syntax as in
  [`query`](https://improverse.github.io/improveR/reference/query.md).

- fetchSize:

  Numeric. Maximum number of results to return. Defaults to 100.

- fetchOffset:

  Numeric. Number of results to skip before returning matches. Defaults
  to 0. Use with `fetchSize` for pagination.

- ident:

  Identifier of the folder to search within. Can be a path, resource id,
  or entity id. Only resources in this folder or its subfolders will be
  returned.

- from:

  Starting point for relative path resolution. Defaults to current
  working directory from
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).
  Only used if `ident` is a relative path.

## Value

A data frame containing the matched resources within the folder
hierarchy, or `NULL` if no resources match, the query contains errors,
the folder doesn't exist, or multiple folders are provided (only one
folder allowed).

## Details

The function loads the specified folder, constructs a path-based filter,
and combines it with your query string. The resulting query searches for
resources where the path equals the folder path or starts with the
folder path followed by `/*`.

This is particularly useful for:

- Searching within a specific project or workflow folder

- Finding resources in a folder hierarchy without knowing exact paths

- Limiting search scope to improve performance and relevance

The function will log a warning and return `NULL` if multiple folders
are provided in `ident`, as only single-folder searches are supported.

## See also

[`query`](https://improverse.github.io/improveR/reference/query.md) for
repository-wide searches,
[`loadChildResources`](https://improverse.github.io/improveR/reference/loadChildResources.md)
to list immediate children of a folder

## Examples

``` r
if (FALSE) { # \dontrun{
# Find all CSV files in a modeling folder
queryFolder(
  ident = "/Modeling/PopPK",
  queryString = "name='*.csv'"
)

# Find finished steps in a workflow
queryFolder(
  ident = "/Workflows/Analysis",
  queryString = "nodeType='Step' AND runStatus='FINISHED'"
)

# Search with pagination
queryFolder(
  ident = "/Data",
  queryString = "nodeType='File'",
  fetchSize = 20,
  fetchOffset = 0
)
} # }
```
