# Lock Resource for Exclusive Editing

Locks a resource on the improve server to prevent concurrent
modifications by other users. This ensures data integrity during editing
operations and prevents conflicts in collaborative pharmaceutical
workflows.

## Usage

``` r
lockResource(ident, from = pwd())
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

  Resource successfully locked by current user

- FALSE:

  Lock failed - resource already locked by another user or server error

## Details

**Lock Behavior:**

- Only one user can hold a lock on a resource at a time

- Lock prevents other users from editing until released with
  [`unlockResource`](https://improverse.github.io/improveR/reference/unlockResource.md)

- Lock is automatically released when session ends or times out

- Requires editable session mode (see
  [`setEditable`](https://improverse.github.io/improveR/reference/setEditable.md))

**Lock Status:** If the resource is already locked, a warning message
indicates which user holds the lock. You must wait for them to unlock or
contact an administrator for lock override.

**Best Practice:** Always unlock resources when finished editing to
avoid blocking collaborators. Use
[`try()`](https://rdrr.io/r/base/try.html) or
[`on.exit()`](https://rdrr.io/r/base/on.exit.html) to ensure unlocking
even if errors occur.

## References

ics1139

## See also

[`unlockResource`](https://improverse.github.io/improveR/reference/unlockResource.md)
to release lock,
[`setEditable`](https://improverse.github.io/improveR/reference/setEditable.md)
to enable write operations,
[`refreshResource`](https://improverse.github.io/improveR/reference/refreshResource.md)
for refreshing resource state

## Examples

``` r
if (FALSE) { # \dontrun{
# Connect and enable editing
improveConnect()
setEditable(TRUE)

# Lock a resource before editing
if (lockResource("MyAnalysis/data.csv")) {
  # Perform editing operations
  # ...

  # Always unlock when done
  unlockResource("MyAnalysis/data.csv")
} else {
  message("Resource is locked by another user")
}

# Safe pattern with automatic unlock
locked <- lockResource("MyAnalysis/results")
if (locked) {
  on.exit(unlockResource("MyAnalysis/results"))
  # Edit operations here
}
} # }
```
