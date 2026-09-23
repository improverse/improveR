# Get Data Table from improve Repository

Retrieves a data table from the improve repository and loads it into R
with metadata including caption and description. Automatically detects
file format and uses appropriate reader (.xls/.xlsx as Excel, .rds as
RDS, .sas7bdat as SAS, others as CSV).

## Usage

``` r
getData(
  ident,
  from = pwd(),
  addAsLink = TRUE,
  caption = "",
  description = "",
  parser = NULL,
  refresh = FALSE,
  ...
)
```

## Arguments

- ident:

  Path, resource ID, entity ID, or entity version ID of the data file.
  Accepts relative paths (starting with `./` or `../`), absolute paths
  (starting with `/`), UUIDs, or improve identifiers. See
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md)
  for full details on identifier formats

- from:

  Root path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md),
  which is the step that initiated the R session (set via
  `IMPROVER_STEP` environment variable)

- addAsLink:

  Logical. If `TRUE` (default), creates a link in the inventory for
  provenance tracking. Use `improveClean()` at workflow end to clean up
  links. Set to `FALSE` for one-off loads without provenance tracking

- caption:

  Custom caption text. If empty string (default), uses "Source (Entity
  version ID): , Last modified on: "

- description:

  Custom description text. If empty string (default), uses the filename

- parser:

  Optional custom parsing function that takes a local file path as first
  argument and returns a data frame. Overrides automatic format
  detection

- refresh:

  If TRUE, invalidate cached file content and resource metadata for
  `ident` before fetching. Defaults to FALSE.

- ...:

  Additional arguments passed to the underlying read function:
  [`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html),
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html),
  [`haven::read_sas()`](https://haven.tidyverse.org/reference/read_sas.html),
  or [`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html)

## Value

A single-row data frame (descriptor) with the following columns, or
`NULL` if the resource cannot be found:

- caption:

  Character. Caption text for display/reporting

- path:

  Character. Local file path (normalized with forward slashes)

- entityId:

  Character. The improve entity ID of the resource

- name:

  Character. Original filename from the repository

- description:

  Character. Description text for the resource

- resource:

  List column containing the full resource metadata data frame from the
  improve server (includes fields like `resourceId`, `entityVersionId`,
  `nodeType`, `lastModifiedOn`, etc.). Access via `result$resource[[1]]`

- data:

  List column containing the parsed data frame. Access the actual data
  via `result$data[[1]]`

- dataType:

  Character. File type detected: "Excel", "RDS", "SAS", "CSV", or
  "Custom parser"

If `ident` matches multiple resources, returns a data frame with
multiple rows (one per resource).

## Details

File format detection is case-insensitive and based on file extension:

- `.xls`, `.xlsx`: Excel via
  [`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html)

- `.rds`: R Data Serialization via
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html)

- `.sas7bdat`: SAS via
  [`haven::read_sas()`](https://haven.tidyverse.org/reference/read_sas.html)

- All others: CSV via
  [`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html) (note:
  `stringsAsFactors` behavior depends on R version)

The returned descriptor integrates with improve's provenance tracking
when `addAsLink = TRUE`. The file is downloaded to a local `data/`
subdirectory in the current workspace.

## References

ics1141

## See also

[`getTextString`](https://improverse.github.io/improveR/reference/getTextString.md)
for loading text files,
[`getGraphics`](https://improverse.github.io/improveR/reference/getGraphics.md)
for loading image files,
[`getR`](https://improverse.github.io/improveR/reference/getR.md) for
loading R objects,
[`loadResource`](https://improverse.github.io/improveR/reference/loadResource.md)
for loading resource metadata without file content

## Examples

``` r
if (FALSE) { # \dontrun{
# Load CSV data from current step
data_desc <- getData("analysis_data.csv")
df <- data_desc$data[[1]]

# Check what was loaded
data_desc$name       # Original filename
data_desc$entityId   # improve entity ID
data_desc$dataType   # "CSV"

# Access full resource metadata
resource_info <- data_desc$resource[[1]]
resource_info$lastModifiedOn

# Load Excel file with custom caption
excel_desc <- getData(
  "results/summary.xlsx",
  caption = "Study Results Summary"
)

# Load without provenance tracking (one-off use)
temp_data <- getData("temp_file.csv", addAsLink = FALSE)

# Use custom parser for special format
custom_desc <- getData(
  "special_format.txt",
  parser = function(path) read.delim(path, sep = "|")
)

# Handle case when resource not found
result <- getData("nonexistent.csv")
if (is.null(result)) {
  message("Resource not found")
}
} # }
```
