# isFileUp2Date

Checks if a new version of the file exists in the repository.

## Usage

``` r
isFileUp2Date(ident, from = pwd())
```

## Arguments

- ident:

  id

- from:

  Used if a relative path is used.

## Value

A single `TRUE` or `FALSE` - never a zero-length value. `TRUE`
unconditionally for an entity version id, because a version cannot
change. Stops with an error, naming the ident and the reason, when
`ident` resolves to no resource, to more than one, to something that is
not a file, or when the server read fails after the local one succeeded
(IMR-287). In a session connected with `persistentCaching = TRUE` the
call reads the server directly and therefore marks the session
non-reproducible (ics1091).

`TRUE` when the version of the file on the server is the one held
locally, `FALSE` otherwise. `TRUE` unconditionally for an entity version
id - a version cannot change. Stops with an error when `ident` resolves
to no resource, and when it resolves to something that is not a file. In
a session connected with `persistentCaching = TRUE` the call reads the
server directly and therefore marks the session non-reproducible
(ics1091).

## Details ident

There are multiple ways to describe the ident of a resource:

Absolute idents:

- resourceId: a UUID

- Data frame: uses the resourceId value of the data frame

- entityId: pointer to the latest version of a resource. Short and long
  entityIds are accepted'

- entityVersionId: pointer to a specific version of a resource. Short
  and long entityIds are accepted'

- path: the full path to a resource, always starting with /.

Relative idents:

- All relative idents are path based, they always have to start with ./
  or ../'

- Relative path without pwd: always starts from the return value of
  pwd()'

- Relative path and pwd as second argument: starts the relative path
  from the absolute ident that was handed over as second argument.
  Sometimes still called “from” but will be updated to pwd.

## References

ics1099
