# Delete Resources Permanently

Permanently removes one or more resources from the repository. Resources
can be folders, analysis trees, workflows, steps, files, or links. For
safety, deletion is prevented if any workflow step within the
resource(s) has an active run status.

## Usage

``` r
delete(res)
```

## Arguments

- res:

  Identifier(s) of resource(s) to delete. Can be a single resource or
  multiple resources. Accepts paths, resource ids, entity ids, or a data
  frame of resources.

## Value

Logical `TRUE` if deletion succeeded, `FALSE` if it failed (e.g.,
resource doesn't exist, step has incompatible runStatus).

## Details

**Safety constraints:** Deletion is blocked if any workflow step within
the target resource(s) has a runStatus other than "INITIAL" or
"FINISHED". This prevents accidental deletion of:

- Running steps (`runStatus = "RUNNING"`)

- Failed steps that may need investigation (`runStatus = "FAILED"`)

- Steps in other intermediate states

For files and links, deletion proceeds immediately as they don't have
run states.

**Important notes:**

- Deletion is **permanent** - deleted resources cannot be recovered

- Parent folder caches are automatically cleared after deletion

- Path caches are invalidated for the deleted resource and any linked
  files

- Repository must be in editable mode (automatically checked)

## References

ics1139

## See also

[`copy`](https://improverse.github.io/improveR/reference/copy.md) to
duplicate resources before deletion,
[`move`](https://improverse.github.io/improveR/reference/move.md) to
relocate resources instead of deleting

## Examples

``` r
if (FALSE) { # \dontrun{
# Delete a single file
delete("/Data/temp_file.csv")

# Delete multiple resources
delete(c("/Temp/file1.csv", "/Temp/file2.csv"))

# Delete a folder and all contents (if step runStatus allows)
delete("/Projects/Old_Analysis")

# Delete using a data frame of resources
temp_files <- query("name='temp_*'")
delete(temp_files)
} # }
```
