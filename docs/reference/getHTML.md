# Get HTML File from improve Repository

Retrieves HTML files (reports, widgets, interactive visualizations) from
the improve repository and returns metadata including the local file
path. The HTML file is downloaded and cached locally for embedding in
RMarkdown reports or viewing in browsers.

## Usage

``` r
getHTML(ident, from = pwd(), addAsLink = TRUE, caption = "", refresh = FALSE)
```

## Arguments

- ident:

  Path, resource ID, or entity ID of the HTML file. Can be a relative
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

- refresh:

  If TRUE, invalidate cached file content and resource metadata for
  `ident` before fetching. Defaults to FALSE.

## Value

A data frame with the following columns:

- caption:

  Character. Source information including entity version ID and last
  modified timestamp

- path:

  Character. Local file system path to the downloaded HTML file

- entityId:

  Character. Entity identifier on improve server

- name:

  Character. Filename of the HTML file

- description:

  Character. Description text (defaults to filename)

- resource:

  List. Complete resource metadata from improve server (29 fields)

## Details

HTML files are cached locally in the `HTML/` subdirectory of the current
working directory. Common use cases include embedding interactive
reports, htmlwidgets output (plotly, leaflet, DT tables), or standalone
HTML visualizations in RMarkdown documents.

Common workflows:

- For RMarkdown embedding: Use
  [`showHTML()`](https://improverse.github.io/improveR/reference/showHTML.md)
  to include HTML inline

- For browser viewing: Use the `path` column to open HTML file

- For htmltools integration: Pass `path` to
  [`htmltools::includeHTML()`](https://rstudio.github.io/htmltools/reference/include.html)

- For metadata only: Access `entityId`, `name`, etc.

## References

ics1141

## See also

[`showHTML`](https://improverse.github.io/improveR/reference/showHTML.md)
for RMarkdown embedding,
[`getData`](https://improverse.github.io/improveR/reference/getData.md)
for loading data tables,
[`getGraphics`](https://improverse.github.io/improveR/reference/getGraphics.md)
for graphics files,
[`getR`](https://improverse.github.io/improveR/reference/getR.md) for R
scripts

## Examples

``` r
if (FALSE) { # \dontrun{
# Get HTML file metadata
report_info <- getHTML("results/interactive_plot.html")
report_path <- report_info$path

# Embed in RMarkdown using helper function
showHTML("results/interactive_plot.html")

# View in browser
html_info <- getHTML("reports/summary.html")
browseURL(html_info$path)

# Use with htmltools
widget_info <- getHTML("widgets/leaflet_map.html")
htmltools::includeHTML(widget_info$path)
} # }
```
