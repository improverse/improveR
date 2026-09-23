# Create a Step Template Environment

Takes an existing step as input and uses it as a blueprint for creating
new steps. The created template can be adapted to specific use cases by
modifying step properties—like description, rationale, parent
relationships, input files, and tool settings—using setter methods. Once
configured, calling `realise()` creates the actual step on the server.
This approach is essential for automated workflows, batch step creation,
and reproducible analyses where you want to script the entire step
creation process.

## Usage

``` r
createStepTemplateEnv(treeIdent = NULL, stepDf = NULL, workflow = NULL)
```

## Arguments

- treeIdent:

  Optional. Assigns a target analysis tree to the template. Can be a
  tree's path, resource ID, or entity ID. If not provided initially, can
  be set later using `setStepTree()`.

- stepDf:

  Optional. Data frame with step metadata from an existing step to use
  as a template. Typically obtained from `getStep(ident)$stepDf`.

- workflow:

  Optional. Workflow environment this template belongs to. Used for
  workflow-level operations and dependencies tracking.

## Value

An environment representing the step template with the following
components:

- stepDf:

  Data frame containing the step configuration

- Setter functions:

  Methods to modify step properties (e.g., `setStepDescription()`,
  `setStepRationale()`, `addStepRemoteFile()`)

- realise():

  Method that transforms the template into an actual step on the server

- run():

  Method to execute the step after creation

- getStepEnv():

  Method to retrieve the full step environment after realization

## Details

The function builds a template through several stages. First, it
generates a unique identifier and creates a data frame to store the step
configuration. Second, it creates an environment containing setter
methods for every configurable property. These methods update the
internal data frame without contacting the server. Third, it attaches a
`realise()` method that transforms the template into an actual step.

When `realise()` is called, it validates the configuration, prepares
process definitions, creates the step via an API request, uploads any
local files, and returns a complete step environment. The template stays
inactive until you call `realise()`, letting you build the complete
configuration first.

Available setter methods include:

- `setStepTree(treeIdent)` - Set the target analysis tree

- `setStepDescription(description)` - Set step description

- `setStepRationale(rationale)` - Set step rationale

- `setStepParent(parentIdent)` - Set parent step

- `addStepRemoteFile(ident, name, asLink, variableName)` - Add input
  files

- `removeStepRemoteFile(ident)` - Remove input files

- `setStepToolInstance(toolInstance)` - Configure execution tool

- `setStepToolFromInstance(toolInstance)` - Configure runserver, tool,
  and tool instance in one call from a
  [`getToolInstances()`](https://improverse.github.io/improveR/reference/getToolInstances.md)
  row

- And many more for complete step configuration control

## See also

[`getStep`](https://improverse.github.io/improveR/reference/getStep.md)
to retrieve existing steps that can serve as templates, `createStep`
which is called internally by `realise()`,
[`importWorkflow`](https://improverse.github.io/improveR/reference/importWorkflow.md)
and
[`exportWorkflow`](https://improverse.github.io/improveR/reference/exportWorkflow.md)
for workflow templates

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a template from an existing step
existingStep <- getStep("/improve-tutorial/Modeling/Step 1")
template <- createStepTemplateEnv(
  treeIdent = "/improve-tutorial/Modeling/",
  stepDf = existingStep$stepDf
)

# Modify the template
template$setStepDescription("New analysis step")
template$setStepRationale("Explore alternative model")

# Add a remote file
template$addStepRemoteFile(
  ident = "/improve-tutorial/EDA/Step 1/data.csv",
  variableName = "inputData"
)

# Realize the step (creates it on the server)
newStep <- template$realise(run = FALSE)

# Create template in a different tree
template2 <- createStepTemplateEnv(stepDf = existingStep$stepDf)
template2$setStepTree("/improve-tutorial/NewAnalysis/")
newStep2 <- template2$realise(run = TRUE)
} # }
```
