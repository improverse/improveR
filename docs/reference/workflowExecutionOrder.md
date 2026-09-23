# Compute execution order for workflow steps based on dependencies

Topological sort of workflow steps: steps with no dependencies come
first, then steps whose dependencies are all already placed,
recursively. Detects cycles by checking whether progress is made each
iteration rather than using a fixed counter, so arbitrarily large
workflows are supported.

## Usage

``` r
workflowExecutionOrder(plan, startSteps = NULL, .prevPlanSize = -1)
```

## Arguments

- plan:

  A data.frame with at least columns `fullName`, `dependencies`
  (comma-separated step names or NA), and `usage` (comma-separated step
  names or NA).

- startSteps:

  Optional data.frame of already-ordered steps (used in recursive
  calls).

- .prevPlanSize:

  Internal parameter to detect stalls. Do not set manually.

## Value

A data.frame of steps in execution order, or NULL if no valid ordering
exists.
