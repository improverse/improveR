# Validate that all tool configurations in a workflow template exist on the server

Iterates all step templates, extracts unique runserver/tool/instance
combos, and verifies each exists on the connected server. Stops with a
clear error listing all missing tools if any are invalid. This catches
tool misconfiguration early, before per-step realise() calls would fail
and cause partial imports.

## Usage

``` r
validateWorkflowTools(workflowTemplate)
```

## Arguments

- workflowTemplate:

  Workflow template environment with \$stepTemplates.

## Value

TRUE invisibly if all tools are valid.
