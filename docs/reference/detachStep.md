# Detach Step from Parent

Removes the hierarchical relationship between a step and its parent,
allowing users to reorganize workflows, isolate steps, or prepare steps
for reassignment to different parents.

## Usage

``` r
detachStep(ident, from = pwd())
```

## Arguments

- ident:

  Identifier of the step to detach. Can be the step's path, resource
  (version) id, full entity (version) id, or short entity (version) id.

- from:

  Root directory for resolving relative paths. Default is
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

The updated step resource, returned invisibly after cache invalidation.

## References

ics1225

## See also

[`attachStep`](https://improverse.github.io/improveR/reference/attachStep.md)
to create parent-child relationships,
[`loadParentStep`](https://improverse.github.io/improveR/reference/loadParentStep.md)
and
[`loadChildSteps`](https://improverse.github.io/improveR/reference/loadChildSteps.md)
for navigating hierarchies,
[`getStep`](https://improverse.github.io/improveR/reference/getStep.md)
for retrieving complete step environments

## Examples

``` r
if (FALSE) { # \dontrun{
# Detach step from its parent
detachStep(ident = "/improve-tutorial/demoSteps/Step 2")

# Verify the step no longer has a parent
loadParentStep("/improve-tutorial/demoSteps/Step 2")
} # }
```
