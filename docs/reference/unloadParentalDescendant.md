# Unload parental/descendant cache for a resource

Remove cached parental/descendant information for a resource identified
by `ident`.

## Usage

``` r
unloadParentalDescendant(ident)
```

## Arguments

- ident:

  id

## Value

Invisibly returns `NULL`.

## Details

This function loads the resource corresponding to `ident` (via
`loadResource`) and then removes any stored parental/descendant data for
that resource from the internal cache. Use this when relationship
information for a resource has changed and the cached copy should be
invalidated.

The function is invoked for its side effect.

## See also

[`refreshParentalDescendant()`](https://improverse.github.io/improveR/reference/refreshParentalDescendant.md),
[`loadParentalDescendant()`](https://improverse.github.io/improveR/reference/loadParentalDescendant.md),
[`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md)

## Examples

``` r
if (FALSE) { # \dontrun{
unloadParentalDescendant(my_ident)
} # }
```
