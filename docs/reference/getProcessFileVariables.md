# Get Process File Variables

Retrieves file variable definitions for a specific process within a
step.

## Usage

``` r
getProcessFileVariables(ident, processId)
```

## Arguments

- ident:

  A step identifier. Can be the step's path, resource (version) id, full
  entity (version) id, or short entity (version) id.

- processId:

  Identifier of the process (main or sub-process) whose variables should
  be returned. Use `loadProcessesForStep` to discover process ids.

## Value

A data frame with one row per process file variable. Columns include:

- id:

  Character. Variable identifier on the server.

- position:

  Integer. Position ordering in the process variable list.

- name:

  Character. Variable name.

- variableType:

  Character. Typically `fileRef` or `filePath`.

- valueResourceId:

  Character. Resource id when `variableType` is `fileRef`.

- processId:

  Character. Process identifier that owns the variable.

Additional columns may be returned depending on server version.

## Details

Process file variables connect a step process to its file inputs.
Variables of type `fileRef` point to improve resources, while `filePath`
variables reference literal paths for the runserver.

## See also

`loadProcessesForStep` to list available processes,
`createProcessFileVariable` to create variables

## Examples

``` r
if (FALSE) { # \dontrun{
processes <- loadProcessesForStep("/improve-tutorial/Modeling/Step 1")
main_process <- processes[processes$processType == "main", ]$id[1]
variables <- getProcessFileVariables(
  ident = "/improve-tutorial/Modeling/Step 1",
  processId = main_process
)
} # }
```
