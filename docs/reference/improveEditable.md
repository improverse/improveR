# Verify Write Mode is Enabled (Guard Function)

Checks that the session is both connected to the improve repository AND
has write mode enabled. Stops with an error if either condition is not
met. Use this as a guard at the start of functions that modify
repository content.

## Usage

``` r
improveEditable()
```

## Value

Invisibly returns `TRUE` if both conditions are met. Throws an error
with a descriptive message if:

- Not connected to the repository (calls
  [`improveConnected`](https://improverse.github.io/improveR/reference/improveConnected.md))

- Connected but write mode is not enabled

## Details

This function serves as a guard clause for write operations. It ensures
that:

1.  [`improveConnect`](https://improverse.github.io/improveR/reference/improveConnect.md)
    has been called successfully

2.  [`setEditable`](https://improverse.github.io/improveR/reference/setEditable.md)`(TRUE)`
    has been called to enable write mode

Package functions that modify repository content should call this
function at their start to fail fast with a clear error message rather
than failing later with an ambiguous API error.

## See also

[`setEditable`](https://improverse.github.io/improveR/reference/setEditable.md)
to enable write mode,
[`improveConnected`](https://improverse.github.io/improveR/reference/improveConnected.md)
to check connection only

## Examples

``` r
if (FALSE) { # \dontrun{
# This function is typically used inside other functions:
my_write_function <- function(path, data) {
  improveEditable()  # Guard: stops if not connected or not editable
  # ... perform write operations ...
}

# Direct usage to check write capability:
improveConnect()
setEditable(TRUE)
improveEditable()  # Passes silently
} # }
```
