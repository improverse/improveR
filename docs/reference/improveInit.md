# Initialize R Modules from improve Repository

Connects to improve, sources a module definition script, and runs its
init function to return a module environment.

## Usage

``` r
improveInit(ident, from = pwd(), logLevel = "INFO", secure = TRUE)
```

## Arguments

- ident:

  Path, resource ID, or entity ID of the module definition script.

- from:

  Root path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

- logLevel:

  Log level passed to
  [`improveConnect`](https://improverse.github.io/improveR/reference/improveConnect.md).

- secure:

  Logical. If `TRUE`, validates certificates when connecting.

## Value

An environment returned by the module's `<module>.init` function, or
`NULL` if initialization fails.

## Details

The function expects a module definition file with a name pattern like
`myModule.R`. After sourcing it, `improveInit()` looks for a function
named `myModule.init` and calls it with the module root path. The root
path is resolved from the parent directory of the module script.

## See also

[`sourceR`](https://improverse.github.io/improveR/reference/sourceR.md)
to source scripts without initialization,
[`improveConnect`](https://improverse.github.io/improveR/reference/improveConnect.md)
to connect explicitly

## Examples

``` r
if (FALSE) { # \dontrun{
# Initialize a module and capture its environment
module_env <- improveInit("modules/myModule.R")
} # }
```
