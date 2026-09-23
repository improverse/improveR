# Attach a Step to a Parent Step

Establishes a hierarchical parent-child relationship between steps by
attaching a step to another step that serves as its parent.

## Usage

``` r
attachStep(ident, parent, from = pwd())
```

## Arguments

- ident:

  Identifier of the step to attach. Can be the step's path, resource
  (version) id, full entity (version) id, or short entity (version) id.

- parent:

  Identifier of the step that will become the parent. Can be the step's
  path, resource (version) id, full entity (version) id, or short entity
  (version) id.

- from:

  Root directory for resolving relative paths. Default is
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

The updated step resource, returned invisibly after cache invalidation.

## References

ics1225

## See also

[`detachStep`](https://improverse.github.io/improveR/reference/detachStep.md)
to remove parent-child relationships,
[`loadParentStep`](https://improverse.github.io/improveR/reference/loadParentStep.md)
and
[`loadChildSteps`](https://improverse.github.io/improveR/reference/loadChildSteps.md)
for navigating hierarchies,
[`getStep`](https://improverse.github.io/improveR/reference/getStep.md)
for retrieving complete step environments

## Examples

``` r
if (FALSE) { # \dontrun{
# Attach Step 2 as a child of Step 1
attachStep(
  ident = "/improve-tutorial/demoSteps/Step 2",
  parent = "/improve-tutorial/demoSteps/Step 1"
)
} # }
```
