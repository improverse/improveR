# Mark Step Flags

Sets or clears boolean flags on an existing step. Only flags that are
explicitly provided (non-`NULL`) are changed; all other step properties
are preserved.

## Usage

``` r
markStep(
  ident,
  from = pwd(),
  keyStep = NULL,
  baseModel = NULL,
  fullModel = NULL,
  finalModel = NULL,
  referenceModel = NULL
)
```

## Arguments

- ident:

  Identifier of the step. Can be the step's path, resource (version) id,
  full entity (version) id, or short entity (version) id.

- from:

  Root directory for resolving relative paths. Default is
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

- keyStep:

  Logical or `NULL`. Mark as a key step.

- baseModel:

  Logical or `NULL`. Mark as a base model.

- fullModel:

  Logical or `NULL`. Mark as a full model.

- finalModel:

  Logical or `NULL`. Mark as the final model.

- referenceModel:

  Logical or `NULL`. Mark as a reference model.

## Value

The updated step resource (invisibly).

## Details

Flags are boolean properties on step resources that help classify and
organise analysis steps within a workflow. They are visible in the
improve client and can be used for filtering and reporting.

Pass `TRUE` to set a flag, `FALSE` to clear it, or leave as `NULL`
(default) to keep the current value.

## See also

[`changeStepDescription`](https://improverse.github.io/improveR/reference/changeStepDescription.md)
for updating step description,
[`changeStepRationale`](https://improverse.github.io/improveR/reference/changeStepRationale.md)
for updating step rationale

## Examples

``` r
if (FALSE) { # \dontrun{
# Mark a step as the final model
markStep("/Projects/analysis/Step 18", finalModel = TRUE)

# Set multiple flags at once
markStep("/Projects/analysis/Step 5", keyStep = TRUE, baseModel = TRUE)

# Clear a flag
markStep("/Projects/analysis/Step 5", baseModel = FALSE)
} # }
```
