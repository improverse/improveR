# Update File Content Prerequisite: must be a file. Multiple files can be updated at once.

Update File Content Prerequisite: must be a file. Multiple files can be
updated at once.

## Usage

``` r
updateFileContent(ident, localPath, comment = "modified by improveRW")
```

## Arguments

- ident:

  Resource(s) to be updated.

- localPath:

  Path to the file with the new content.

- comment:

  Commit comment. Defaults to "modified by improveRW".

## Value

The refreshed resource after the upload, as
[`refreshResource()`](https://improverse.github.io/improveR/reference/refreshResource.md)
returns it. `NULL` when `ident` resolves to no resource or the upload
itself failed. For several idents the refreshed resources of all of
them, merged.

## References

ics1210
