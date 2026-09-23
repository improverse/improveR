# Unlock Resource to Allow Collaborative Access

Releases a lock on a resource, allowing other users to edit it. This
should be called after completing editing operations to restore
collaborative access in pharmaceutical workflows.

## Usage

``` r
unlockResource(ident, from = pwd())
```

## Arguments

- ident:

  Resource identifier (entityId, resourceId, or path). See
  [`common_ident`](https://improverse.github.io/improveR/reference/common_ident.md)
  for supported identifier formats.

- from:

  Base path for resolving relative paths. Defaults to current working
  directory from
  [`pwd`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

Logical value:

- TRUE:

  Resource successfully unlocked

- FALSE:

  Unlock failed - resource not locked or server error

## Details

**Unlock Behavior:**

- Only the user who locked the resource (or administrator) can unlock it

- Unlocking restores collaborative access for all team members

- Requires editable session mode (see
  [`setEditable`](https://improverse.github.io/improveR/reference/setEditable.md))

- Attempting to unlock an already-unlocked resource returns FALSE with a
  warning

**Lock Ownership:** If you attempt to unlock a resource locked by
another user, the operation will fail. Contact an administrator for
force-unlock if the original user is unavailable.

**Best Practice:** Always unlock resources promptly after editing to
avoid blocking collaborators. Use
[`on.exit()`](https://rdrr.io/r/base/on.exit.html) in functions to
ensure unlock happens even if errors occur.

**Warning:** Do not force-unlock resources while another user is
actively editing, as this may cause data conflicts or loss of unsaved
changes.

## References

ics1139

## See also

[`lockResource`](https://improverse.github.io/improveR/reference/lockResource.md)
to acquire lock,
[`setEditable`](https://improverse.github.io/improveR/reference/setEditable.md)
to enable write operations,
[`refreshResource`](https://improverse.github.io/improveR/reference/refreshResource.md)
for refreshing resource state

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic lock/unlock pattern
improveConnect()
setEditable(TRUE)

lockResource("MyAnalysis/data.csv")
# ... perform edits ...
unlockResource("MyAnalysis/data.csv")

# Safe pattern with automatic unlock on function exit
edit_resource <- function(path) {
  if (!lockResource(path)) {
    stop("Could not acquire lock")
  }
  on.exit(unlockResource(path))

  # Edit operations here - unlock happens automatically
  # even if an error occurs
}

# Check unlock status
if (unlockResource("MyAnalysis/results")) {
  message("Resource unlocked successfully")
} else {
  message("Resource was not locked or unlock failed")
}
} # }
```
