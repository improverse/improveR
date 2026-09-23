# Push CLI

Pushes local changes to the improve server. After pushing, the resource
cache is invalidated to ensure subsequent queries reflect the updated
server state.

## Usage

``` r
pushCli(
  localPath,
  comment = NULL,
  force = FALSE,
  preview = FALSE,
  includeFiles = NULL,
  excludeFiles = NULL,
  files = NULL,
  json = FALSE
)
```

## Arguments

- localPath:

  The local repository path.

- comment:

  Optional commit comment.

- force:

  Logical. If `TRUE`, overwrite remote changes (skip conflict check).

- preview:

  Logical. If `TRUE`, preview only without writing to the server.

- includeFiles:

  Character. Comma-separated glob patterns to include.

- excludeFiles:

  Character. Comma-separated glob patterns to exclude.

- files:

  Character vector. Optional specific file/directory paths to push.

- json:

  Logical. If `TRUE`, requests JSON output and returns a parsed R list
  (useful for `preview = TRUE`).

## Value

CLI output: character vector of stdout lines when `json = FALSE`, or a
parsed list when `json = TRUE`. Returned invisibly.
