# Get Step Environment

Returns a detailed environment object containing a step's properties,
relationships with other steps, and methods for working with the step.
This environment provides the foundational interface for complex
workflow operations, dependencies tracking, and analytical pipeline
execution.

## Usage

``` r
getStep(ident, workflow = NULL)
```

## Arguments

- ident:

  Step identifier. Can be the step's path, resource (version) id, full
  entity (version) id, or short entity (version) id.

- workflow:

  Optional workflow environment to add the step to. If not provided, a
  new workflow is created.

## Value

An environment representing the step with the following components:

- stepDf:

  Data frame containing step metadata and configuration

- children, parent, usage, dependencies:

  Nested environments with lazy-loading for step relationships

- workflow:

  Workflow environment for orchestration and dependencies tracking

- getStepInventory():

  Method to retrieve the step's file inventory

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

The function loads step information from the server, creates a workflow
environment for tracking dependencies, and builds a comprehensive step
environment. Navigation through the returned environment is facilitated
by R's `$` operator and supports autocompletion in RStudio, Positron,
and VS Code with radian.

## References

ics1213

## See also

[`createStepEnv`](https://improverse.github.io/improveR/reference/createStepEnv.md)
for the underlying environment builder,
[`createStepTemplateEnv`](https://improverse.github.io/improveR/reference/createStepTemplateEnv.md)
for creating new steps,
[`loadChildSteps`](https://improverse.github.io/improveR/reference/loadChildSteps.md)
and
[`loadParentStep`](https://improverse.github.io/improveR/reference/loadParentStep.md)
for navigation,
[`runStepResource`](https://improverse.github.io/improveR/reference/runStepResource.md)
and
[`finishRunResource`](https://improverse.github.io/improveR/reference/finishRunResource.md)
for execution

## Examples

``` r
if (FALSE) { # \dontrun{
# Get a step environment
step <- getStep("/improve-tutorial/Modeling/Step 1")

# Access step metadata
step$stepDf

# Load and navigate to children
step$children$load()
ls(step$children)

# Get step inventory
inventory <- step$getStepInventory()
} # }
```
