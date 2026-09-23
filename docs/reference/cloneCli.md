# Clone CLI Resource

Clones a resource from the CLI.

## Usage

``` r
cloneCli(ident, localPath)
```

## Arguments

- ident:

  The identifier of the resource to clone.

- localPath:

  The local path where the resource will be cloned.

## Value

The CLI's output, invisibly: a character vector of the lines it wrote to
stdout and stderr. A non-zero exit code is logged as a warning, not
raised, so a returned value is no proof that the clone succeeded - check
the local path. Stops with an error when `ident` does not exist in the
repository.
