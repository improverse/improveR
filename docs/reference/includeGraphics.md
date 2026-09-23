# Include Graphics in Knitr Chunk

wrapper around
[`knitr::include_graphics`](https://rdrr.io/pkg/knitr/man/include_graphics.html)
that automatically handles file retrieval from improve, caching, and
metadata display. Best used within RMarkdown code chunks.

## Usage

``` r
includeGraphics(
  ident,
  from = pwd(),
  addAsLink = TRUE,
  caption = "",
  description = "",
  includeCaption = T,
  includeDescription = T,
  refresh = FALSE,
  ...
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
  timestamp

- description:

  Custom description text. If empty, defaults to filename

- includeCaption:

  Logical. If `TRUE` (default), sets the chunk option `fig.cap` to the
  caption text

- includeDescription:

  Logical. If `TRUE` (default), prints the description text below the
  image

- refresh:

  If TRUE, invalidate cached file content and resource metadata for
  `ident` before fetching. Defaults to FALSE.

- ...:

  Additional arguments passed to
  [`knitr::include_graphics`](https://rdrr.io/pkg/knitr/man/include_graphics.html)

## Value

The result of
[`knitr::include_graphics`](https://rdrr.io/pkg/knitr/man/include_graphics.html),
which renders the image in the RMarkdown output.

## References

ics1141

## See also

[`getGraphics`](https://improverse.github.io/improveR/reference/getGraphics.md)
for underlying retrieval function,
[`showGraphics`](https://improverse.github.io/improveR/reference/showGraphics.md)
for raw Markdown syntax generation
