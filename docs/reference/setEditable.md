# Enable or Disable Repository Write Mode

Controls whether write operations (create, update, delete) are permitted
on the improve repository. By default, connections are read-only for
safety. Call this function with `TRUE` to enable modifications.

## Usage

``` r
setEditable(editable = T)
```

## Arguments

- editable:

  Logical. If `TRUE` (default), enables write operations to the
  repository. If `FALSE`, restricts the session to read-only operations.

## Value

Invisibly returns `NULL`. Called for its side effect of setting the
editable flag in the session cache.

## Details

**Warning:** Enabling write mode allows operations that modify
repository content, including:

- Creating new resources, steps, and workflows

- Updating existing files and metadata

- Deleting resources

The editable state persists for the duration of the R session or until
explicitly changed by calling this function again.

## See also

[`improveEditable`](https://improverse.github.io/improveR/reference/improveEditable.md)
to check and enforce write mode,
[`improveConnect`](https://improverse.github.io/improveR/reference/improveConnect.md)
to establish connection

## Examples

``` r
if (FALSE) { # \dontrun{
# Connect to improve server (read-only by default)
improveConnect()

# Enable write operations
setEditable(TRUE)

# Perform modifications...
# createFile(...)

# Disable write operations when done
setEditable(FALSE)
} # }
```
