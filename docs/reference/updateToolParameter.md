# Update a Tool Parameter Value

Updates the value of an existing parameter on a tool instance.

## Usage

``` r
updateToolParameter(
  runserverId,
  instanceId,
  parameterId,
  value,
  parameterLovId = NULL
)
```

## Arguments

- runserverId:

  Character. The ID of the runserver.

- instanceId:

  Character. The ID of the tool instance.

- parameterId:

  Character. The ID of the parameter to update.

- value:

  Character. The new parameter value.

- parameterLovId:

  Character or NULL. The parameter LOV ID. If NULL, the existing
  parameter is fetched to determine it.

## Value

A list with the updated parameter details, or `NULL` on failure.

## References

ccs39

## See also

[`createToolParameter`](https://improverse.github.io/improveR/reference/createToolParameter.md),
[`createToolInstance`](https://improverse.github.io/improveR/reference/createToolInstance.md)
