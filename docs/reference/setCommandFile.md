# Set Command File on a Realised Step

Binds a file as the command-file process variable on an already-realised
step. This is used when a step has been created and realised, but the
command file needs to be set or changed after the fact.

## Usage

``` r
setCommandFile(stepIdent, fileIdent, from = pwd())
```

## Arguments

- stepIdent:

  Identifier of the step. Can be a path, resource ID, entity ID, or a
  data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- fileIdent:

  Identifier of the file to bind as command file. Can be a path,
  resource ID, entity ID, or a data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

The updated process variables data frame, invisibly. Returns `NULL` on
failure.

## References

ccs40

## See also

[`getMainProcess`](https://improverse.github.io/improveR/reference/getMainProcess.md),
[`getProcessFileVariables`](https://improverse.github.io/improveR/reference/getProcessFileVariables.md)

## Examples

``` r
if (FALSE) { # \dontrun{
setCommandFile("/Projects/MyTree/Step 1", "/Projects/Files/myScript.R")
} # }
```
