# Load the parental/descendant relationships of a resource

Returns the parental descendants of the resource. Parental descendants
are

- copies (modified or not) of the resource to other locations

- output files which were created by the resource due to its role as an
  input file.

## Usage

``` r
loadParentalDescendant(ident, from = pwd())
```

## Arguments

- ident:

  Character. Resource identifier to resolve (e.g. UUID, name, or any
  identifier accepted by
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md)).

- from:

  Character. Path used to resolve the identifier (defaults to the
  current working directory via
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md)).

## Value

A dataframe with the following columns:

- `type` (character)

- `resourceId` (character)

- `entityId` (character)

- `entityVersionId` (character)

- `path` (character)

- `name` (character)

- `data` (list)

The `data` column contains a nested dataframe with:

- `resourceId` (character)

- `resourceVersionId` (character)

- `nodeType` (character)

- `name` (character)

- `deleted` (logical)

- `entityId` (character)

- `entityVersionId` (character)

- `revisionId` (character)

- `lastModifiedOn` (numeric)

- `lastModifiedByName` (character)

- `status` (character)

- `fileSize` (integer)

- `hasChildren` (logical)

- `hasChildrenIncludingFiles` (logical)

- `path` (character)

- `outdatedLink` (logical)

- `workingFile` (logical)

- `lastModifiedOnDate` (POSIXct)

## Details

The function uses caching to improve performance. The cache persists for
the entire R session. If parental descendant relationships change on the
server, use
[`refreshParentalDescendant()`](https://improverse.github.io/improveR/reference/refreshParentalDescendant.md)
to refresh the cached data, or use
[`unloadParentalDescendant()`](https://improverse.github.io/improveR/reference/unloadParentalDescendant.md)
to remove the cached data.

## See also

[`refreshParentalDescendant()`](https://improverse.github.io/improveR/reference/refreshParentalDescendant.md),
[`unloadParentalDescendant()`](https://improverse.github.io/improveR/reference/unloadParentalDescendant.md),
[`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# by UUID
loadParentalDescendant("123e4567-e89b-12d3-a456-426614174000")

# by resource name within a specific repository path
loadParentalDescendant("my-resource-name", from = "/path/to/repo")
} # }
```
