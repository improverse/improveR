# improveR Package Development Notes

**Date:** 2026-01-08
**Topic:** getR() File Caching Issue During Package Development

## Issue Summary

The `getR()` function has a caching behavior that conflicts with `devtools::load_all()` during package development, causing `load_all()` to fail with file not found errors.

## Technical Details

### How getR() Works

When `getR()` retrieves an R script or RDS file from the improve repository, it:

1. Downloads the file from the improve server
2. Caches it locally to `<pwd()>/R/` directory
3. Returns a data frame with metadata including the `path` to the cached file
4. The cached filename includes the entity ID to avoid collisions (e.g., `envhost1.hc.scintecodev.internal-6111_FI-61177_manipulate.R`)

This behavior is hardcoded in `R/getR.R` line 68:

```r
getAbstract(ident=ident, ..., folderName = "R", ...)
```

The underlying `getAbstract()` function (in `R/getGeneric.R`) always downloads files to `<pwd()>/<folderName>/`.

### Why This Breaks Package Development

During package development:

* `pwd()` returns the package root directory (e.g., `C:/dev/git-repos/improver-base/improveR/`)
* Cached files are created in `R/` alongside package function definitions
* When `devtools::load_all()` runs, it sources **all** `.R` files in the `R/` directory
* Cached R scripts often contain executable code (e.g., `read.csv("mtcars.csv")`)
* This executable code runs during `load_all()`, causing errors if dependencies are missing

**Error Example:**

```
Error in `load_all()`:
! Failed to load R/envhost1.hc.scintecodev.internal-6111_FI-61177_manipulate.R
Caused by error in `file()`:
! cannot open the connection
```

### Design Intent vs. Reality

The `R/` cache directory is **intentional design** for workflow execution:

* During workflow runs, `pwd()` points to the step workspace
* Cached files in `<step_workspace>/R/` are isolated from other steps
* The `resetStep()` function (line 215 in `getGeneric.R`) explicitly lists `"R"` as a content folder to clean

However, this design assumes `getR()` is called from within a workflow step, not from the package root during development.

## Workarounds for Package Developers

### Option 1: Clean Before load_all()

```r
# Delete all cached files before loading
unlink("R/*hc.scintecodev.internal*.R")
devtools::load_all()
```

### Option 2: Add to .Rbuildignore

Add pattern to exclude cached files from package builds:

```
^R/.*\.hc\..*\.R$
^R/envhost.*\.R$
```

### Option 3: Test from Workflow Context

Don't test `getR()` from package root - test it from an actual workflow step where `pwd()` is the step workspace.

## Recommended Documentation Updates

The `getR()` documentation should include a prominent warning:

1. **@details section**: Add explicit warning about package development conflict
2. **Side effects**: Clearly document that files are created in `R/` subdirectory
3. **Workarounds**: List the options above for package developers
4. **Note**: Emphasize this is intended for workflow execution, not interactive development

## Related Functions

The same issue affects other `get*()` functions that use `getAbstract()`:

* `getData()` - caches to `data/` (safer - not sourced by load_all)
* `getGraphics()` - caches to `graphics/`
* `getHTML()` - caches to `html/`
* `getFile()` - default `data/`, but user-configurable via `folderName` parameter
* `getCopy()` - caches to current directory (`.`)

Only `getR()` has the problematic hardcoded `folderName = "R"`.

## Potential Long-Term Solutions

1. **Make folderName configurable** in `getR()` like `getFile()` does
2. **Detect package development context** and use alternative cache location
3. **Add .Rbuildignore automatically** during package setup
4. **Use temp directory** for cached files instead of R/ during development

## Files Examined

* `R/getR.R` - Main function with hardcoded `folderName = "R"`
* `R/getGeneric.R` - Contains `getAbstract()` implementation
* `R/getData.R` - Comparison function (uses `folderName = "data"`)

---


---

**Date:** 2026-01-09
**Topic:** `whoami()` Returns NULL Due to Missing User in Connection Configuration

## Issue Summary

The `whoami()` function returns `NULL` with warnings when called after connecting to the improve platform using environment variable-based authentication. The root cause is that `improveConnect()` ignores the `IMPROVER_USER` environment variable when building the internal configuration.

## Symptoms

When calling `whoami()` after a successful connection:

```
> improveR::whoami()
2026-01-09 11:47:09.720155 WARNING::auditTrail resource by ID: root:root-root could not be loaded
Warning message:
In max(audit$createdAt) : no non-missing arguments to max; returning -Inf
NULL
```

Despite `Sys.getenv("IMPROVER_USER")` correctly returning `"admin"`.

## Technical Details

### How whoami() Works

The `whoami()` function in `R/whoami.R` uses a two-tier approach:

1. **Primary**: Returns `conf()$user` if it contains a non-empty string
2. **Fallback**: Queries the audit trail of the current step to find who created the workflow run

When `conf()$user` is empty, the fallback attempts to load an audit trail using `pwd()$entityId`. If the user is at the repository root (`pwd()$path: /`), this results in attempting to load `root:root-root`, which doesn't exist.

### The Bug Location

In `R/improveConnect.R` at line 223, the configuration data frame is built with a hardcoded empty user field:

```r
confData <- data.frame(
  repoUrl = serverAddress,
  runWorkspace = runWorkspace,
  user = "",  # <-- BUG: Hardcoded empty string
  reqToken = reqToken,
  stepId = stepId,
  stringsAsFactors = F
)
```

Meanwhile, the OAuth authentication flow in `R/oauthLogin.R` (line 63) correctly sets the environment variable:

```r
Sys.setenv(IMPROVER_USER = user)
```

And `R/tokenManagement.R` (lines 78-79) also handles `IMPROVER_USER` when reading token data:

```r
if (!is.null(tokenData$IMPROVER_USER)) {
  Sys.setenv(IMPROVER_USER = tokenData$IMPROVER_USER)
}
```

The disconnect is that the environment variable authentication path in `improveConnect()` never reads this value back into the configuration.

## Solution

Change line 223 in `R/improveConnect.R` from:

```r
user = ""
```

to:

```r
user = Sys.getenv("IMPROVER_USER", "")
```

This ensures that if `IMPROVER_USER` is set (either by OAuth login, token management, or manually), it will be picked up by the connection configuration and available to `whoami()`.

## Files Examined

* `R/whoami.R` - Function that fails when `conf()$user` is empty
* `R/improveConnect.R` - Contains the bug at line 223
* `R/oauthLogin.R` - Correctly sets `IMPROVER_USER` after OAuth
* `R/tokenManagement.R` - Correctly handles `IMPROVER_USER` in token data

---


---

**Date:** 2026-01-09
**Topic:** `lockResource()` and `unlockResource()` Crash with "Argument is of Length Zero" Error

## Issue Summary

Both `lockResource()` and `unlockResource()` functions crash with "argument is of length zero" error when the REST API returns an unexpected response structure. The functions attempted to check HTTP status codes without first verifying that the response object and its `status_code` field exist.

## Symptoms

When calling `lockResource()` with a valid resource identifier:

```
> lockResource("envhost1.hc.scintecodev.internal-6111:AT-42408-1")
Error in `if (result$status_code == 200) ...`:
! argument is of length zero
```

The same error occurs with `unlockResource()` under similar conditions.

## How to Reproduce

1. Connect to the improve platform and enable editing:
   ```r
   improveConnect()
   setEditable(TRUE)
   ```
2. Call `lockResource()` with any valid resource identifier when the API returns an unexpected response structure:
   ```r
   lockResource("envhost1.hc.scintecodev.internal-6111:AT-42408-1")
   ```
3. **Expected**: Function returns `FALSE` with appropriate warning message
4. **Actual**: Function crashes with "argument is of length zero" error

## Technical Details

### How Lock/Unlock Functions Work

Both functions follow the same pattern:

1. Validate that session is in editable mode via `improveEditable()`
2. Update resource metadata via `updateResource()` to check current lock status
3. Make REST API call to lock/unlock endpoint
4. Check HTTP status code and return `TRUE` (success) or `FALSE` (failure)

### The Bug Location

In `R/lockUnlock.R`, both functions had unsafe status code checks:

**lockResource() at line 78 (original code):**
```r
result <- authenticatedREST("/resources/{resourceId}/lock",
                            urlParams = list(resourceId=res$resourceId),
                            restType = "PUT")
if (result$status_code==200) {  # <-- BUG: No null check
  return(T)
}
return(F)
```

**unlockResource() at line 169 (original code):**
```r
result <- authenticatedREST("/resources/{resourceId}/unlock",
                            urlParams = list(resourceId=res$resourceId),
                            restType = "PUT")
if (result$status_code==200) {  # <-- BUG: No null check
  return(T)
}
return(F)
```

### Root Cause: R's Behavior with NULL Comparisons

When `authenticatedREST()` returns a result where `result$status_code` is `NULL` or doesn't exist:

1. The expression `NULL == 200` evaluates to `logical(0)` (a zero-length logical vector), **not** `FALSE`
2. The `if()` statement requires a length-1 logical value
3. R throws the error: "argument is of length zero"

This happens because:
* R's comparison operators are vectorized
* Comparing `NULL` to any value produces an empty logical vector
* `if()` cannot evaluate an empty vector

## Solution

Add proper null checks using short-circuit evaluation before accessing `status_code`:

**Fixed lockResource() at line 78:**
```r
if (!is.null(result) && !is.null(result$status_code) && result$status_code==200) {
  return(T)
}
return(F)
```

**Fixed unlockResource() at line 169:**
```r
if (!is.null(result) && !is.null(result$status_code) && result$status_code==200) {
  return(T)
}
return(F)
```

The `&&` operator evaluates left-to-right and short-circuits at the first `FALSE`, preventing the `result$status_code == 200` comparison from executing when `result` or `result$status_code` is `NULL`.

## Files Examined

* `R/lockUnlock.R` - Contains both bugs at lines 78 and 169

---

**Date:** 2026-01-10
**Topic:** Variable Shadowing Bug in Workflow Execution Order Logic

## Issue Summary

The `executionOrderInternal()` private function in `createWorkflow.R` contained a variable shadowing bug where a loop variable was inadvertently overwritten by an inner variable with the same name. This caused incorrect workflow execution plan generation when determining which steps could be added to the execution order based on satisfied dependencies.

## Symptoms

The execution plan generation would incorrectly attempt to remove steps from the plan using the wrong identifier, potentially:

* Failing to remove the correct dependent step from the pending plan
* Causing infinite loops in execution order determination (hitting the 500-iteration safety limit)
* Generating incorrect execution sequences for workflows with multiple interdependent steps

## How to Reproduce

1. Create a workflow with multiple steps that have complex dependency chains
2. Call `createReexecutionPlan()` or `createFullExecutionPlan()` on the workflow
3. The execution order logic iterates through steps, attempting to determine which dependent steps can be added based on satisfied dependencies
4. **Expected**: Steps are correctly added to execution order when all their dependencies are satisfied
5. **Actual**: With the bug, the wrong step identifier is used when removing steps from the pending plan

## Technical Details

### How executionOrderInternal() Works

The `executionOrderInternal()` function (lines 89-135) is a recursive function that builds the workflow execution order by:

1. Starting with steps that have no dependencies (`is.na(plan$dependencies)`)
2. For each starting step, examining which other steps use its outputs (via the `usage` field)
3. For each dependent step, checking if all of **its** dependencies are now satisfied
4. If satisfied, adding the dependent step to the execution order and removing it from the pending plan
5. Recursively continuing until all steps are ordered or a cycle is detected

### The Bug Location / Root Cause

In the nested loop starting at line 105, there was a variable naming collision:

**Original buggy code (lines 109-122):**
```r
for (dependencies in dependenciess) {
  dependenciesHandle <- plan[plan$fullName == dependencies, ]
  if (
    nrow(dependenciesHandle) == 1 &&
      "dependencies" %in% names(dependenciesHandle)
  ) {
    dependencies <- strsplit(           # <-- BUG: Overwrites loop variable
      dependenciesHandle$dependencies,
      ",",
      fixed = TRUE
    )[[1]]
    if (all(dependencies %in% startSteps$fullName)) {
      startSteps <- plyr::rbind.fill(startSteps, dependenciesHandle)
      plan <- plan[plan$fullName != dependencies, ]  # <-- Uses wrong value
    }
  }
}
```

**The problem:**
1. **Line 109**: Loop variable named `dependencies` (should represent one dependent step name)
2. **Line 115**: Inner variable also named `dependencies` (represents an array of dependency names)
3. **Line 122**: Attempts to remove step from plan using `dependencies`, but this now holds an array of strings instead of the single dependent step name from the loop
4. The comparison `plan$fullName != dependencies` with a vector fails to match correctly

## Solution

Rename both variables to be more descriptive and avoid the collision:

**Fixed code (lines 109-122):**
```r
for (dependentStepName in dependenciess) {           # Renamed loop var
  dependenciesHandle <- plan[plan$fullName == dependentStepName, ]
  if (
    nrow(dependenciesHandle) == 1 &&
      "dependencies" %in% names(dependenciesHandle)
  ) {
    stepDependencies <- strsplit(                    # Renamed inner var
      dependenciesHandle$dependencies,
      ",",
      fixed = TRUE
    )[[1]]
    if (all(stepDependencies %in% startSteps$fullName)) {
      startSteps <- plyr::rbind.fill(startSteps, dependenciesHandle)
      plan <- plan[plan$fullName != dependentStepName, ]  # Now correct
    }
  }
}
```

**Changes:**
* Loop variable: `dependencies` → `dependentStepName` (more descriptive and accurate)
* Inner variable: `dependencies` → `stepDependencies` (more descriptive and avoids collision)
* Line 122 now correctly uses `dependentStepName` to remove the right step from the plan

## Files Examined

* `R/createWorkflow.R` - Contains the bug in `.workflow_private$executionOrderInternal()` at lines 109-122

---

