# refreshHistory

refreshHistory

## Usage

``` r
refreshHistory(ident)

updateHistory(...)
```

## Arguments

- ident:

  id

- ...:

  For backwards compatibility with the deprecated `update*` alias; not
  used by `refresh*` itself.

## Value

The freshly read history, in the same shape as
[`loadHistory()`](https://improverse.github.io/improveR/reference/loadHistory.md) -
the cached copy is dropped first, so the value comes from the server.

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

ics1094
