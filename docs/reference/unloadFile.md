# unloadFile

Removes a file from cache.

## Usage

``` r
unloadFile(ident, from = pwd(), filePath = ".", addIdToName = FALSE)
```

## Arguments

- ident:

  id

- from:

  used if a relative path is used

- filePath:

  local Path where the file should be stored, relative to rootPath,
  normally wd

- addIdToName:

  logical, if the entityId should be added to the filename

## Value

No meaningful value - called for its side effects: the downloaded copy
is deleted from the workspace and both the resource and the file content
are dropped from the cache. Does nothing when `ident` resolves to no
resource.

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
