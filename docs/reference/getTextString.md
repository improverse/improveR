# Get Text File as Character String

Retrieves a text file from the improve repository and loads it as a
character string with metadata including caption and description. All
lines are collapsed into a single string with newline separators.

## Usage

``` r
getTextString(
  ident,
  from = pwd(),
  addAsLink = TRUE,
  caption = "",
  description = "",
  refresh = FALSE,
  ...
)
```

## Arguments

- ident:

  Path, resource ID, or entity ID of the text file. Can be a relative
  path (from current step), absolute path, or improve identifier

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

- refresh:

  If TRUE, invalidate cached file content and resource metadata for
  `ident` before fetching. Defaults to FALSE.

- ...:

  Additional arguments passed to
  [`readLines()`](https://rdrr.io/r/base/readLines.html)

## Value

A list object (descriptor) with the following components:

- `data`: Character string containing the complete file contents with
  lines collapsed using newline (`\n`) separators

- `dataType`: Character string set to "Text"

- `path`: Character string with local file path

- `caption`: Character string with caption text

- `description`: Character string with description text

- Additional metadata fields from the resource descriptor

## Details

The function reads all lines from the text file using
[`readLines()`](https://rdrr.io/r/base/readLines.html) and collapses
them into a single character string. The returned descriptor integrates
with improve's provenance tracking when `addAsLink = TRUE`. Access the
text content via `result$data`.

## References

ics1141

## See also

[`getData`](https://improverse.github.io/improveR/reference/getData.md)
for loading data tables,
[`getR`](https://improverse.github.io/improveR/reference/getR.md) for
loading R objects,
[`getHTML`](https://improverse.github.io/improveR/reference/getHTML.md)
for loading HTML files

## Examples

``` r
if (FALSE) { # \dontrun{
# Load text file from current step
text_desc <- getTextString("report_text.txt")
content <- text_desc$data  # Extract text string

# Load file with custom metadata
script_desc <- getTextString(
  "scripts/analysis.R",
  caption = "Main Analysis Script",
  description = "Primary statistical analysis code"
)
} # }
```
