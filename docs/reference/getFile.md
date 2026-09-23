# Get a File Object

Get a File Object

## Usage

``` r
getFile(
  ident,
  from = pwd(),
  addAsLink = TRUE,
  caption = "",
  description = "",
  folderName = "data",
  addIdToName = T,
  refresh = FALSE
)
```

## Arguments

- ident:

  Path, resource, or entity ID of the file.

- from:

  Used for relative paths. By default, pwd is used (initiated with the
  step that started improveR).

- addAsLink:

  Creates a link in the inventory if TRUE; improveClean must be run at
  the end.

- caption:

  By default, entity ID and last modified are the caption; alternative
  text can be provided here.

- description:

  By default, the filename is the description; alternative text can be
  provided here.

- folderName:

  By default, `data` is the subfolder in the workspace where the file is
  created.

- addIdToName:

  By default, T prepends the entity ID to the name to avoid collisions.

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
