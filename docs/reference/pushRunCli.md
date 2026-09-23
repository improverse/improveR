# Push Run CLI

Pushes local changes and creates a run for the cloned step. After
pushing, the resource cache is invalidated.

## Usage

``` r
pushRunCli(
  localPath,
  command = "Rstudio",
  comment = NULL,
  force = FALSE,
  preview = FALSE,
  includeFiles = NULL,
  excludeFiles = NULL,
  runserverUrl = NULL,
  commandArgs = NULL,
  json = FALSE
)
```

## Arguments

- localPath:

  The local repository path.

- command:

  The command executable to run. Default is "Rstudio".

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

- runserverUrl:

  Character. Runserver URL (defaults to local IP).

- commandArgs:

  Character vector. Additional command arguments. Use for arguments that
  start with `-`.

- json:

  Logical. If `TRUE`, requests JSON output and returns a parsed R list
  (useful for `preview = TRUE`).

## Value

CLI output: character vector of stdout lines when `json = FALSE`, or a
parsed list when `json = TRUE`. Returned invisibly.
