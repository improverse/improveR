# Includes HTML in rMarkdown, creates a string that has to be outputted

Includes HTML in rMarkdown, creates a string that has to be outputted

## Usage

``` r
showHTML(
  ident,
  from = pwd(),
  addAsLink = TRUE,
  caption = "",
  includeCaption = T,
  refresh = FALSE
)
```

## Arguments

- ident:

  path, resource or entity ID of the picture

- from:

  used for relative paths, by default pwd is used, which is initiated
  with the step that started improveR

- addAsLink:

  creates a link in inventory if TRUE, warn improveClean has to be run
  at the end

- caption:

  by default entityID and lastModified are the caption, here alternative
  text can be provided

- includeCaption:

  display the caption

- refresh:

  If TRUE, invalidate cached file content and resource metadata for
  `ident` before fetching. Defaults to FALSE.

## Value

An `htmltools` tag list holding the HTML file and its caption, ready to
be printed from an R Markdown chunk. When `ident` resolves to several
objects, a single character string with their rendered output
concatenated. `NULL` when no HTML object was found.

## References

ics1141
