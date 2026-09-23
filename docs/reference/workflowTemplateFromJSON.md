# Load workflow template from JSON file

Loads a workflow template that was saved with toJSON(), including all
step definitions, internal links, and parameter definitions.

## Usage

``` r
workflowTemplateFromJSON(filepath)
```

## Arguments

- filepath:

  Path to the JSON file to load

## Value

A workflow template environment with all parameters and steps

## Examples

``` r
if (FALSE) { # \dontrun{
# Save a template
workflowTemplate$toJSON("my_template.json")

# Load it back
loadedTemplate <- workflowTemplateFromJSON("my_template.json")

# Set parameters and realize
loadedTemplate$setParameter("inputDataset", "newFileId")
realizedWorkflow <- loadedTemplate$realise()
} # }
```
