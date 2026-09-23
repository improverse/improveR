# Create a workflow template environment from an existing workflow

Extracts all steps, relationships, parameters, and files from the
workflow and builds a reusable workflow template environment.

## Usage

``` r
createWorkflowTemplateEnv(workflow = NULL, addParental = F)
```

## Arguments

- workflow:

  The workflow environment to template. If NULL (default), creates an
  empty workflow template.

- addParental:

  If TRUE, the created stepTemplates have the steps from the workflow as
  parents (default: FALSE).

## Value

An environment representing the workflow template with the following
methods:

- addStepTemplate(stepTemplate, name=NULL):

  Adds a step template to the workflow template.

- createExecutionPlan():

  Creates a data.frame describing the execution plan for the workflow.

- df():

  Returns a data.frame with metadata for all steps in the template.

- executePlan(executionPlan):

  Executes a given execution plan data.frame.

- listParameters():

  Returns a data frame of all defined parameters.

- parameterizeStep(paramName, stepPattern, property, target=NULL,
  required=TRUE, defaultValue=NULL):

  Registers a parameter to modify steps before realisation.

  paramName

  :   Name of the parameter.

  stepPattern

  :   Pattern to match steps. Three matching modes are supported:

      - `"*"` matches all steps in the workflow template.

      - Prefix pattern ending with `*` (e.g., `"analysisTree1/*"`)
        matches all steps whose fullName starts with the prefix.

      - Exact match (e.g., `"import data"`) first tries to match the
        step's fullName, then falls back to matching the step's
        description.

  property

  :   The property to parameterize: "remoteFile", "localFile",
      "treeIdent", "gridArgument.", "description", "rationale", or
      "stepName".

  target

  :   (Optional) Target within the property, e.g., the path of a remote
      file.

  required

  :   (Optional) Whether this parameter must be set before realization
      (default: TRUE).

  defaultValue

  :   (Optional) Default value if not set.

- realise():

  Executes the plan, applying parameters and creating the workflow
  steps.

- setParameter(paramName, value):

  Sets a value for a defined parameter.

- setWorkflowTreeIdent(treeIdent, from=pwd()):

  Sets the ident of the location/analysis tree where the workflow
  template should be created, i.e., realised. If not defined, the new
  workflow will be located in the same location(s) where the step(s) of
  the template workflow are located.

- setWorkflowTreeName(treeName):

  Sets the name of the location/analysis tree where the workflow
  template should be created, i.e., realised. If not defined, the new
  workflow will be located in the same location(s) where the step(s) of
  the template workflow are located.

- setWorkflowTreeRootFolder(rootFolder):

  Sets the root folder path for the workflow tree where the template is
  realised to.

- toJSON(filepath, pretty=TRUE):

  Saves the template definition to a JSON file.

- validateParameters():

  Checks if all required parameters are set, throwing an error if not.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get a step and load its dependencies
step <- getStep("/myProject/workflow/analysisTree/Step 1")
step$dependencies$load()

# Create a workflow template from the step's workflow
workflowTemplate <- createWorkflowTemplateEnv(step$workflow)

# View the steps in the template
workflowTemplate$df()

# Define a parameter to modify the description of a specific step
workflowTemplate$parameterizeStep(
  stepPattern = "import data", #pattern that matches the name or the description of the step which the paramter should target
  paramName = "inputDescription", #name of the parameter to which a value will be asigned to
  property = "description" #the property of the targeted step which will be modified by the parameter
)

# Define a parameter that applies to all steps
workflowTemplate$parameterizeStep(
  stepPattern = "*", 
  paramName = "allStepsRationale",
  property = "rationale"
)

# Set the parameter values
workflowTemplate$setParameter("inputDescription", "Load input dataset")
workflowTemplate$setParameter("allStepsRationale", "Automated workflow execution")

# Review parameters before realisation
workflowTemplate$listParameters()

# Optionally set a new target tree for the realised workflow
workflowTemplate$setWorkflowTreeName("newAnalysisTree")

# Realise the template (creates actual steps)
realisedWorkflow <- workflowTemplate$realise()
} # }
```
