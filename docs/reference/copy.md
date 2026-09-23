# Copy Resources to Target Location

Duplicates one or more resources to a target folder, creating
independent copies with their own version history. Unlike
[`createLink`](https://improverse.github.io/improveR/reference/createLink.md),
copies are fully independent and changes to the original do not affect
the copy.

## Usage

``` r
copy(
  sources,
  target,
  targetName = "",
  overwrite = F,
  comment = "modified by improveRW"
)
```

## Arguments

- sources:

  Identifier(s) of resource(s) to copy. Can be a single resource or
  multiple resources. Accepts paths, resource ids, entity ids, or a data
  frame of resources.

- target:

  Identifier of the target folder or file location. Can be a path,
  resource id, or entity id. If copying multiple sources, must be a
  container (folder).

- targetName:

  Character. Optional new name for the copied resource. Only used when
  copying a single source. Ignored when copying multiple sources.
  Defaults to empty string (uses original name).

- overwrite:

  Logical. If `TRUE` and a resource with the same name exists at the
  target location, it is deleted and replaced. If `FALSE` (default) and
  a name conflict exists, returns `NULL` with a warning.

- comment:

  Character. Commit message for the copy operation. Defaults to
  "modified by improveRW".

## Value

The copied resource(s) as a data frame, or `NULL` if the operation fails
(e.g., source doesn't exist, target doesn't exist, name conflict with
overwrite=FALSE, invalid target type for the source type).

## Details

The function supports both single and batch copying:

- **Single copy:** When copying one resource, you can optionally specify
  a new name via `targetName`

- **Batch copy:** When copying multiple resources, all are copied to the
  target folder with their original names (`targetName` must be empty)

The copy operation validates:

- Source and target resources exist

- Target accepts the source type (e.g., can't copy a workflow into a
  file)

- No naming conflicts (unless `overwrite=TRUE`)

- Repository is in editable mode (automatically checked)

After copying, parent folder caches are automatically cleared to ensure
fresh data on subsequent queries.

## References

ics1139

## See also

[`move`](https://improverse.github.io/improveR/reference/move.md) to
relocate resources instead of copying,
[`createLink`](https://improverse.github.io/improveR/reference/createLink.md)
to create lightweight references instead of full copies,
[`delete`](https://improverse.github.io/improveR/reference/delete.md) to
remove resources

## Examples

``` r
if (FALSE) { # \dontrun{
# Copy a single file to a folder
copy(
  sources = "/Data/baseline.csv",
  target = "/Modeling/PopPK"
)

# Copy with a new name
copy(
  sources = "/Data/baseline.csv",
  target = "/Modeling/PopPK",
  targetName = "input_data.csv"
)

# Copy multiple files at once
copy(
  sources = c("/Data/file1.csv", "/Data/file2.csv"),
  target = "/Analysis"
)

# Overwrite existing file
copy(
  sources = "/Data/new_baseline.csv",
  target = "/Modeling/baseline.csv",
  overwrite = TRUE
)
} # }
```
