# Is Resource Up2 Date

Checks whether the cached resource is the version the server holds.

## Usage

``` r
isResourceUp2Date(ident, from = pwd())
```

## Arguments

- ident:

  id

- from:

  pwd for relative path

## Value

A single `TRUE` or `FALSE` - never a zero-length value. `TRUE`
unconditionally for an entity version id, because a version cannot
change. Stops with an error, naming the ident and the REST status, when
`ident` resolves to no resource, to more than one, or when the server
read fails after the local one succeeded (IMR-287). In a session
connected with `persistentCaching = TRUE` the call reads the server
directly and therefore marks the session non-reproducible (ics1091).

## References

ics1090
