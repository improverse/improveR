# Unload Resource

Unloads a resource.

## Usage

``` r
unloadResource(ident, from = pwd())
```

## Arguments

- ident:

  id

- from:

  pwd for relative path

## Value

No meaningful value - called for its side effect of dropping the
resource from the cache; a version is dropped from the version cache,
everything else from the resource cache. Does nothing when `ident`
resolves to no resource.

## References

ics1090
