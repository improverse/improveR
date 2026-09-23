# Update a Tool Instance

Updates the command of an existing tool instance on a runserver.

## Usage

``` r
updateToolInstance(runserverId, instanceId, command)
```

## Arguments

- runserverId:

  Character. The ID of the runserver.

- instanceId:

  Character. The ID of the tool instance.

- command:

  Character. The new command string.

## Value

A list with the updated instance details, or `NULL` on failure.

## References

ccs37

## See also

[`createToolInstance`](https://improverse.github.io/improveR/reference/createToolInstance.md),
[`getToolInstances`](https://improverse.github.io/improveR/reference/getToolInstances.md)
