# Get a Local Copy of a File

Get a Local Copy of a File

## Usage

``` r
getCopy(ident, from = pwd(), caption = "", description = "", refresh = FALSE)
```

## Arguments

- ident:

  Path, resource, or entity ID of the file.

- from:

  Used for relative paths. By default, pwd is used (initiated with the
  step that started improveR).

- caption:

  By default, entity ID and last modified are the caption; alternative
  text can be provided here.

- description:

  By default, the filename is the description; alternative text can be
  provided here.

- refresh:

  If TRUE, invalidate cached file content and resource metadata for
  `ident` before fetching. Defaults to FALSE.

## Value

A data frame with one row per file, holding `caption`, `path` (the
absolute path of the local copy), `entityId`, `name`, `description` and
the resource itself in `resource`. `NULL` when `ident` resolves to
nothing usable - the get functions accept only files and links, and
anything else is logged and skipped.

## References

ics1141
