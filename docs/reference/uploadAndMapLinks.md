# Upload link files and build link mapping environment

Upload link files and build link mapping environment

## Usage

``` r
uploadAndMapLinks(importFolder, zipFile, baseName, importRepoFolderResource)
```

## Arguments

- importFolder:

  Path to extracted import directory.

- zipFile:

  Path to original zip file (for finding mapping file).

- baseName:

  Base name for mapping file lookup.

- importRepoFolderResource:

  Resource for the target folder.

## Value

An environment mapping version keys to entity IDs.
