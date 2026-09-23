# Load Child Steps

Loads all child steps for a given Step resource identifier. Only works
on Step resources - returns NULL for other resource types. Child steps
represent hierarchical workflow structures. Results are cached for
performance.

## Usage

``` r
loadChildSteps(ident, from = pwd())
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

A data frame with class "improveResource" containing:

- `type` - Type indicator ("childSteps")

- `resourceId`, `entityId`, `entityVersionId` - Resource identifiers

- `path`, `name` - Resource location and name

- `data` - A list containing data frames with 30 variables per child
  step:

  - Standard metadata: `resourceId`, `resourceVersionId`, `nodeType`,
    `name`, `path`, `parentId`, `deleted`, `entityId`,
    `entityVersionId`, `fullEntityId`, `fullEntityVersionId`,
    `revisionId`

  - Audit fields: `lastModifiedOn`, `lastModifiedById`,
    `lastModifiedByName`, `lastModifiedOnDate` (POSIXct)

  - Hierarchy: `hasChildren`, `hasChildrenIncludingFiles`

  - Status: `outdatedLink`, `finishedStatus`, `workingFile`, `fileSize`

  - Execution: `runStatus` (e.g., "FINISHED"), `runResult` (e.g.,
    "CANCELED", "SUCCESS")

  - Step classification: `keyStep`, `baseModel`, `fullModel`,
    `finalModel`, `referenceModel` (logical)

  - `children` - Nested list containing tibbles of child resources
    (Files, etc.) with 21 variables including `status`, `fileHash`

Returns NULL if the resource is not a Step.

## Details

This function only works on Step resources. If called on a non-Step
resource (e.g., Folder, Analysis Tree), it will log a warning and return
NULL.

The nested `children` field provides access to files and other resources
within each child step, enabling complete workflow hierarchy traversal.

## References

ics1205

## See also

[`unloadChildSteps`](https://improverse.github.io/improveR/reference/unloadChildSteps.md)
to clear cache,
[`refreshChildSteps`](https://improverse.github.io/improveR/reference/refreshChildSteps.md)
to refresh from server,
[`loadChildResources`](https://improverse.github.io/improveR/reference/loadChildResources.md)
for all child resource types

## Examples

``` r
if (FALSE) { # \dontrun{
# Load child steps of current step
childSteps <- loadChildSteps(pwd())
childSteps$data[[1]]

# Access nested children (files) of first child step
files <- childSteps$data[[1]]$children[[1]]

# Load child steps by path
childSteps <- loadChildSteps("/Projects/MyWorkflow/MainStep")
} # }
```
