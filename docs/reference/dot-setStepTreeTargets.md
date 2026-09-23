# Set tree targets for each step template during folder import

Maps each step to its correct analysis tree in the target repository.

## Usage

``` r
.setStepTreeTargets(workflowTemplate, treesManifest, targetResource, idMapping)
```

## Arguments

- workflowTemplate:

  The workflow template environment.

- treesManifest:

  Data frame of tree entries from manifest.

- targetResource:

  Root target folder resource.

- idMapping:

  Environment mapping old to new resource IDs.
