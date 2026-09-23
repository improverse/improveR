# Create Step Environment

Constructs a specialized R environment object that encapsulates a step's
complete context and provides programmatic access to all step-related
data and operations. This environment serves as a comprehensive
interface containing the step's metadata, file relationships, navigation
capabilities, and workflow context.

## Usage

``` r
createStepEnv(stepDf = NULL, workflow = NULL)
```

## Arguments

- stepDf:

  Data frame with step metadata from an existing step.

- workflow:

  Workflow environment this step belongs to.

## Value

An environment representing the step with the following components:

- stepDf:

  Data frame containing the step configuration and metadata

- children, parent, usage, dependencies:

  Nested environments with lazy-loading for step relationships

- getStepInventory():

  Method to retrieve step inventory

- getStepResource():

  Method to get the step resource object

- getStepState():

  Method to get the step's execution state

- getStepWithoutCache():

  Method to get fresh step data from server

- retrieveMainProcess():

  Method to get the main process configuration

- createTemplate():

  Method to create a template from this step

## Details

This function is typically called internally by
[`getStep`](https://improverse.github.io/improveR/reference/getStep.md),
which users should use instead of calling `createStepEnv` directly. The
environment-based approach enables intuitive navigation through workflow
hierarchies using R's `$` operator.

## See also

[`getStep`](https://improverse.github.io/improveR/reference/getStep.md)
for the user-facing function that calls this internally,
[`createStepTemplateEnv`](https://improverse.github.io/improveR/reference/createStepTemplateEnv.md)
for creating new step templates,
[`loadChildSteps`](https://improverse.github.io/improveR/reference/loadChildSteps.md)
and
[`loadParentStep`](https://improverse.github.io/improveR/reference/loadParentStep.md)
for navigation
