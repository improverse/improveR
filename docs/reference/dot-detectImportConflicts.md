# Detect import conflicts in target folder

Scans the target folder recursively for resources that would conflict
with the import manifest entries.

## Usage

``` r
.detectImportConflicts(folderStructure, treesManifest, targetResource)
```

## Arguments

- folderStructure:

  Data frame of folder structure from manifest.

- treesManifest:

  Data frame of trees from manifest.

- targetResource:

  The target folder resource.

## Value

Character vector of conflict descriptions (empty if no conflicts).
