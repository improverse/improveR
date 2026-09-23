# Validate mapping files before import

Validates LinkMapping.json and ToolMapping.json files. Stops import if
invalid mappings are found.

## Usage

``` r
validateMappingFiles(zipFile, baseName)
```

## Arguments

- zipFile:

  Path to the zip file being imported.

- baseName:

  Name stem used for mapping file lookup.

## Value

TRUE invisibly if all validations pass.
