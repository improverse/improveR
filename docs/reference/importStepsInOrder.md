# Import steps in execution order: realise, push files, bind variables

This is the core import loop shared by importWorkflow and importFolder.

## Usage

``` r
importStepsInOrder(orderedWorkflow, workflowTemplate, importWF, importFolder)
```

## Arguments

- orderedWorkflow:

  Data frame of steps in execution order.

- workflowTemplate:

  Workflow template environment.

- importWF:

  Data frame from workflow.json (modified in place with newEntityId).

- importFolder:

  Path to extracted import directory.

## Value

Modified importWF with newEntityId column populated.
