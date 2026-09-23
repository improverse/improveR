# Load Child Resources

Loads all child resources for a given resource identifier. Child
resources can be folders, steps, workflows and analysis trees, but also
the elements of a step's inventory. Results are cached for performance.

## Usage

``` r
loadChildResources(ident, from = pwd())
```

## Arguments

- ident:

  The resource identifier - can be a resource ID, entity ID, entity
  version ID, or path. Can also be a list of identifiers.

- from:

  Reference point for relative paths. Defaults to current working
  directory via
  [`pwd`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A list with class "improveResource" containing:

- `type` - Type indicator ("child")

- `resourceId` - Unique resource identifier

- `entityId` - Entity identifier

- `entityVersionId` - Entity version identifier

- `path` - Full resource path

- `name` - Resource name

- `data` - A list containing data frames of child resources. Each data
  frame contains columns with resource metadata:

  - `resourceId`, `resourceVersionId` - Resource identifiers

  - `nodeType` - Type of resource (e.g., "Step", "Analysis Tree",
    "File", "Folder")

  - `name`, `path` - Resource name and full path

  - `parentId` - Parent resource identifier

  - `deleted` - Logical indicating deletion status

  - `entityId`, `entityVersionId` - Entity identifiers

  - `fullEntityId`, `fullEntityVersionId` - Full entity URLs

  - `revisionId` - Revision identifier

  - `createdByName`, `createdById` - Creator information

  - `createdAt`, `createdAtDate` - Creation timestamp (milliseconds and
    POSIXct)

  - `lastModifiedByName`, `lastModifiedById` - Last modifier information

  - `lastModifiedOn`, `lastModifiedOnDate` - Last modification timestamp

  - `fileSize` - File size in bytes

  - `hasChildren`, `hasChildrenIncludingFiles` - Child resource
    indicators

  - `outdatedLink` - Logical indicating outdated link status

  - `finishedStatus` - Execution status (e.g., "unfinished")

  - `workingFile` - Logical indicating working file status

  Additional fields for specific node types:

  - For Steps: `keyStep`, `baseModel`, `fullModel`, `finalModel`,
    `referenceModel`, `ownedById`, `ownedByName`

  - For Files: `status`, `fileHash`

Dates are converted to POSIXct format via
[`convertImproveTimestampToPosix`](https://improverse.github.io/improveR/reference/convertImproveTimestampToPosix.md).

## See also

[`unloadChildResources`](https://improverse.github.io/improveR/reference/unloadChildResources.md)
to clear cache,
[`refreshChildResources`](https://improverse.github.io/improveR/reference/refreshChildResources.md)
to refresh from server,
[`loadChildSteps`](https://improverse.github.io/improveR/reference/loadChildSteps.md)
for step-specific children

## Examples

``` r
if (FALSE) { # \dontrun{
# Load children of current step
children <- loadChildResources(pwd())
children$data[[1]]

# Load children by path
children <- loadChildResources("/Projects/MyWorkflow")
} # }
```
