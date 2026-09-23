# Create a Tool Parameter on an Instance

Creates a parameter on a tool instance. The parameter is identified by
name (e.g. "Tool Arguments") which is resolved to a parameter LOV ID.
Idempotent: if the parameter already exists, it is updated instead.

## Usage

``` r
createToolParameter(runserverId, instanceId, parameterName, value)
```

## Arguments

- runserverId:

  Character. The ID of the runserver.

- instanceId:

  Character. The ID of the tool instance.

- parameterName:

  Character. The parameter name (from parameterLov, e.g. "Tool
  Arguments").

- value:

  Character. The parameter value.

## Value

A list with the parameter details (id, parameterLovId, value), or `NULL`
on failure.

## References

ccs38

## See also

[`updateToolParameter`](https://improverse.github.io/improveR/reference/updateToolParameter.md),
[`createToolInstance`](https://improverse.github.io/improveR/reference/createToolInstance.md)
