# Generate Markdown Syntax for Graphics

Creates the Markdown syntax required to display a graphics file in an
RMarkdown document. Can optionally include caption and description text.

## Usage

``` r
showGraphics(
  ident,
  from = pwd(),
  addAsLink = TRUE,
  caption = "",
  description = "",
  includeCaption = T,
  includeDescription = T,
  refresh = FALSE
)
```

## Arguments

- ident:

  Path, resource ID, or entity ID of the graphics file. Can be a
  relative path (from current step), absolute path, or improve
  identifier

- from:

  Root path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md)
  (current step location)

- addAsLink:

  Logical. If `TRUE`, creates a link in the inventory for provenance
  tracking. Use `improveClean()` at workflow end to clean up links

- caption:

  Custom caption text. If empty, defaults to entityID and lastModified
  timestamp. To suppress caption, set `includeCaption = FALSE`

- description:

  Custom description text. If empty, defaults to filename. To suppress
  description, set `includeDescription = FALSE`

- includeCaption:

  Logical. If `TRUE` (default), includes the caption in the generated
  Markdown

- includeDescription:

  Logical. If `TRUE` (default), includes the description in the
  generated Markdown (as title text)

- refresh:

  If TRUE, invalidate cached file content and resource metadata for
  `ident` before fetching. Defaults to FALSE.

## Value

A character string containing the formatted Markdown syntax:
`![caption](path 'description')` Returns empty string if file not found.

## References

ics1141

## See also

[`getGraphics`](https://improverse.github.io/improveR/reference/getGraphics.md)
for underlying retrieval function,
[`includeGraphics`](https://improverse.github.io/improveR/reference/includeGraphics.md)
for Knitr integration
