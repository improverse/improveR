# Get R Script or RDS Object from improve Repository

Retrieves an R script (.R) or R data object (.rds) from the improve
repository and returns metadata including the local file path. The file
is downloaded but not loaded into memory - use
[`sourceR()`](https://improverse.github.io/improveR/reference/sourceR.md)
to execute scripts or [`readRDS()`](https://rdrr.io/r/base/readRDS.html)
on the returned path to load RDS objects.

## Usage

``` r
getR(
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

  Path, resource ID, or entity ID of the R file. Can be a relative path
  (from current step), absolute path, or improve identifier

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

  Character. Local file system path to the downloaded R file. Use this
  with [`source()`](https://rdrr.io/r/base/source.html) or
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html)

- entityId:

  Character. Entity identifier on improve server

- name:

  Character. Filename of the R script or RDS object

- description:

  Character. Description text (defaults to filename)

- resource:

  List. Complete resource metadata from improve server (29 fields)

## Details

Unlike
[`getData()`](https://improverse.github.io/improveR/reference/getData.md)
which loads data into memory, `getR()` only downloads the R file and
returns its metadata. The file is cached locally at the path specified
in the `path` column.

Common workflows:

- For R scripts: Use
  [`sourceR()`](https://improverse.github.io/improveR/reference/sourceR.md)
  to execute, or `source(result$path)`

- For RDS objects: Use `readRDS(result$path)` to load the object

- For metadata only: Access `result$entityId`, `result$name`, etc.

## References

ics1141

## See also

[`sourceR`](https://improverse.github.io/improveR/reference/sourceR.md)
for executing R scripts directly,
[`getData`](https://improverse.github.io/improveR/reference/getData.md)
for loading data tables,
[`improveInit`](https://improverse.github.io/improveR/reference/improveInit.md)
for initializing R modules

## Examples

``` r
if (FALSE) { # \dontrun{
# Get R script metadata
script_info <- getR("analysis/prepare_data.R")
script_path <- script_info$path

# Load and execute the script
source(script_path)

# Or use sourceR() helper (automatically sources)
sourceR("analysis/prepare_data.R")

# Get RDS object
model_info <- getR("models/final_model.rds")
model <- readRDS(model_info$path)
} # }
```
