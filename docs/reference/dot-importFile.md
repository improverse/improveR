# Import a single file with onConflict handling

Creates a file or handles the conflict based on onConflict mode.

## Usage

``` r
.importFile(parentResource, fileName, localPath, onConflict)
```

## Arguments

- parentResource:

  The parent folder resource.

- fileName:

  Name of the file.

- localPath:

  Local path to the file content.

- onConflict:

  One of "skip", "overwrite", "error".

## Value

The file resource, or NULL on failure.
