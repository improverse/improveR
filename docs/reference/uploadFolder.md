# Upload a Complete Folder Prerequisite: must be a folder.

Upload a Complete Folder Prerequisite: must be a folder.

## Usage

``` r
uploadFolder(targetIdent, localFolder, comment = "modified by improveRW")
```

## Arguments

- targetIdent:

  Target resource.

- localFolder:

  Path to the local folder to upload.

- comment:

  Commit comment. Defaults to "modified by improveRW".

## Value

No meaningful value - called for its side effect of creating the folder
under `targetIdent` and filling it, recursively, with what `localFolder`
holds. When `localFolder` is not a directory a warning is logged and
nothing is created.

## References

ics1210
