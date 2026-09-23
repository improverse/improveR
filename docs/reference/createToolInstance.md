# Create a Tool Instance on a Runserver

Creates a new tool instance on the specified runserver. Idempotent: if
an instance with the same toolId and name already exists, it is
returned.

## Usage

``` r
createToolInstance(
  runserverId,
  toolId,
  instanceName,
  command = "",
  gridProvider = "",
  gridProviderInstance = ""
)
```

## Arguments

- runserverId:

  Character. The ID of the runserver.

- toolId:

  Character. The ID of the tool.

- instanceName:

  Character. The name for the tool instance.

- command:

  Character. The command string. Default `""`.

- gridProvider:

  Character. Grid provider type (e.g. "DOCKER"). Default `""`.

- gridProviderInstance:

  Character. Grid provider instance name. Default `""`.

## Value

A list with the instance details (id, name, toolId, runserverId), or
`NULL` on failure.

## References

ccs36

## See also

[`updateToolInstance`](https://improverse.github.io/improveR/reference/updateToolInstance.md),
[`getToolInstances`](https://improverse.github.io/improveR/reference/getToolInstances.md)
