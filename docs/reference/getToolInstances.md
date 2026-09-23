# Get Tool Instances

Returns an environment containing all available tool instances with
their configurations. Tools orchestrate step execution by defining how
scripts are run, which external software to use (R, NONMEM, Monolix),
and how to handle inputs and outputs.

## Usage

``` r
getToolInstances()
```

## Value

An environment where each tool instance is accessible by its full name.
Each tool instance contains:

- toolId:

  Unique identifier for the tool

- runserverId:

  ID of the runserver hosting the tool

- url:

  URL of the tool service

- parameters:

  Data frame of tool parameters and their values

- gridArguments:

  Data frame of grid computing arguments (if applicable)

## Details

The function checks for cached tool instances first, returning them
immediately if available. Otherwise, it loads tool configuration data
from the server, including runservers, tools, parameters, and grid
arguments. Results are cached for performance. When tool configurations
change, call
[`resetToolInstances`](https://improverse.github.io/improveR/reference/resetToolInstances.md)
to clear the cache.

Tool instances are referenced in `stepDf$processes` within step
environments created by
[`getStep()`](https://improverse.github.io/improveR/reference/getStep.md),
determining how steps execute.

## See also

[`resetToolInstances`](https://improverse.github.io/improveR/reference/resetToolInstances.md)
to clear the tool instance cache,
[`getMainProcess`](https://improverse.github.io/improveR/reference/getMainProcess.md)
to retrieve the tool instance for a specific step,
[`getStep`](https://improverse.github.io/improveR/reference/getStep.md)
for step environments that reference tool instances

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all tool instances
tools <- getToolInstances()

# List available tools
ls(tools)

# Access a specific tool instance
rTool <- tools$`R R R runserver`

# View tool parameters
rTool$parameters
} # }
```
