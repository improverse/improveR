# Change Step Rationale

Modifies the rationale field of an existing step, allowing users to
update the reasoning and justification for why analytical steps were
created and included in the workflow.

## Usage

``` r
changeStepRationale(ident, from = pwd(), rationale)
```

## Arguments

- ident:

  Identifier of the step. Can be the step's path, resource (version) id,
  full entity (version) id, or short entity (version) id.

- from:

  Root directory for resolving relative paths. Default is
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

- rationale:

  New step rationale text.

## Value

The updated step resource, returned invisibly after cache refresh.

## References

ics1217

## See also

[`changeStepDescription`](https://improverse.github.io/improveR/reference/changeStepDescription.md)
for updating step description,
[`getStep`](https://improverse.github.io/improveR/reference/getStep.md)
for retrieving step information

## Examples

``` r
if (FALSE) { # \dontrun{
# Update step rationale
changeStepRationale(
  ident = "/improve-tutorial/Modeling/Step 1",
  rationale = "Investigate linear relationship between variables"
)
} # }
```
