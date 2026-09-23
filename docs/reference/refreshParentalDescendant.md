# Refresh parental/descendant cache for a resource

Replace cached parental/descendant information for a resource identified
by `ident` with newly fetched data.

## Usage

``` r
refreshParentalDescendant(ident)

updateParentalDescendant(...)
```

## Arguments

- ident:

  id

- ...:

  For backwards compatibility with the deprecated `update*` alias; not
  used by `refresh*` itself.

## Value

Invisibly returns `NULL`.

## Details

This function loads the resource corresponding to `ident` (via
`loadResource`) and then removes any stored parental/descendant data for
that resource from the internal cache. Use this when relationship
information for a resource has changed and the cached copy should be
invalidated so subsequent operations will recompute or reload up-to-date
relationship data.

The function is invoked for its side effect.

## See also

[`loadParentalDescendant()`](https://improverse.github.io/improveR/reference/loadParentalDescendant.md),
[`unloadParentalDescendant()`](https://improverse.github.io/improveR/reference/unloadParentalDescendant.md),
[`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md)

## Examples

``` r
if (FALSE) { # \dontrun{
refreshParentalDescendant(my_ident)
} # }
```
