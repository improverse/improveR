# Change Step Description

Modifies the description field of an existing step, allowing users to
update documentation and clarify the purpose of analytical steps as
analyses evolve.

## Usage

``` r
changeStepDescription(ident, from = pwd(), description)
```

## Arguments

- ident:

  Identifier of the step. Can be the step's path, resource (version) id,
  full entity (version) id, or short entity (version) id.

- from:

  Root directory for resolving relative paths. Default is
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

- description:

  New step description text.

## Value

The updated step resource, returned invisibly after cache refresh.

## References

ics1217

## See also

[`changeStepRationale`](https://improverse.github.io/improveR/reference/changeStepRationale.md)
for updating step rationale,
[`getStep`](https://improverse.github.io/improveR/reference/getStep.md)
for retrieving step information

## Examples

``` r
if (FALSE) { # \dontrun{
# Update step description
changeStepDescription(
  ident = "/improve-tutorial/Modeling/Step 1",
  description = "Initial exploratory analysis"
)
} # }
```
