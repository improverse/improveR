# Source R Scripts from improve Repository

Downloads and sources R scripts stored in the improve repository. If the
target is a folder or list of resources, the function recursively
sources each script.

## Usage

``` r
sourceR(ident, from = pwd(), addAsLink = TRUE, refresh = FALSE)
```

## Arguments

- ident:

  Path, resource ID, or entity ID of the R script.

- from:

  Root path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

- addAsLink:

  Logical. If `TRUE`, creates a link in the inventory for provenance
  tracking. Use `improveClean()` at workflow end to clean up links.

- refresh:

  If TRUE, invalidate cached file content and resource metadata for
  `ident` before fetching. Defaults to FALSE.

## Value

Invisibly returns `NULL`. Scripts are sourced for their side effects.

## Details

This function wraps
[`getR`](https://improverse.github.io/improveR/reference/getR.md) to
download the script, then calls
[`source()`](https://rdrr.io/r/base/source.html) on the local file path.
When multiple resources are returned, each is sourced in sequence.
Sourcing executes code in the current session, so ensure the script
content is trusted and compatible with your environment.

## References

ics1141

## See also

[`getR`](https://improverse.github.io/improveR/reference/getR.md) to
download scripts or RDS metadata,
[`improveInit`](https://improverse.github.io/improveR/reference/improveInit.md)
for module initialization

## Examples

``` r
if (FALSE) { # \dontrun{
# Source a single script
sourceR("analysis/prepare_data.R")
} # }
```
