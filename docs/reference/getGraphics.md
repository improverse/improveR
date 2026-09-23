# Get Graphics File from improve Repository

Retrieves graphics files (images, plots, figures) from the improve
repository and returns metadata including the local file path. The
graphics file is downloaded and cached locally for use in reports,
visualizations, or further processing.

## Usage

``` r
getGraphics(
  ident,
  from = pwd(),
  addAsLink = TRUE,
  caption = "",
  description = "",
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
  timestamp

- description:

  Custom description text. If empty, defaults to filename

- refresh:

  If TRUE, invalidate cached file content and resource metadata for
  `ident` before fetching. Defaults to FALSE.

## Value

A data frame with the following columns:

- caption:

  Character. Source information including entity version ID and last
  modified timestamp

- path:

  Character. Local file system path to the downloaded graphics file

- entityId:

  Character. Entity identifier on improve server

- name:

  Character. Filename of the graphics file

- description:

  Character. Description text (defaults to filename)

- resource:

  List. Complete resource metadata from improve server (29 fields)

## Details

Graphics files are cached locally in the `graphics/` subdirectory of the
current working directory. The file is downloaded but not loaded into
R - use the returned `path` with appropriate display functions.

Common workflows:

- For RMarkdown: Use
  [`showGraphics()`](https://improverse.github.io/improveR/reference/showGraphics.md)
  to generate markdown syntax

- For knitr chunks: Use
  [`includeGraphics()`](https://improverse.github.io/improveR/reference/includeGraphics.md)
  with knitr integration

- For custom display: Use the `path` column with graphics functions

- For metadata only: Access `entityId`, `name`, etc.

## References

ics1141

## See also

[`showGraphics`](https://improverse.github.io/improveR/reference/showGraphics.md)
for RMarkdown display,
[`includeGraphics`](https://improverse.github.io/improveR/reference/includeGraphics.md)
for knitr integration,
[`getData`](https://improverse.github.io/improveR/reference/getData.md)
for loading data tables,
[`getR`](https://improverse.github.io/improveR/reference/getR.md) for R
scripts

## Examples

``` r
if (FALSE) { # \dontrun{
# Get graphics file metadata
plot_info <- getGraphics("results/efficacy_plot.png")
plot_path <- plot_info$path

# Display in RMarkdown using helper function
showGraphics("results/efficacy_plot.png")

# Display in knitr chunk
includeGraphics("results/efficacy_plot.png")

# Custom display with base R
img_info <- getGraphics("figures/diagram.png")
# Use img_info$path with your preferred display method
} # }
```
