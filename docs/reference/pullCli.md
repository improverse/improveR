# Pull CLI

Pulls changes from the CLI. After pulling, the resource cache is
invalidated to ensure the in-memory state matches what was just
downloaded.

## Usage

``` r
pullCli(localPath, preview = FALSE)
```

## Arguments

- localPath:

  The local repository path.

- preview:

  Logical. If TRUE, only preview what would be pulled.

## Value

With `preview = TRUE`, a data frame of what a pull would fetch,
invisibly: one row per file with `path` and `change` (`"added"`,
`"modified"` or `"deleted"`), and zero rows when the clone is up to
date. The preview is computed in improveR from the clone's own baseline
and one read of the resource; the CLI is not called and nothing is
written (IMR-283).

Otherwise the CLI's output, invisibly: a character vector of the lines
it wrote to stdout and stderr. A non-zero exit code is logged as a
warning, not raised, so a returned value is no proof that the pull
succeeded. The caches for the local repository's resource and its
children are dropped afterwards; if the repository cannot be identified
that step is skipped and a warning says so.
