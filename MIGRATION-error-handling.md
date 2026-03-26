# improveR Error Handling Migration Guide

This document describes the error handling changes introduced in this version
and what downstream code needs to adapt.

## Summary of Changes

### 1. `authenticatedREST` no longer throws exceptions

**Before:** `authenticatedREST()` had an `ignoreFail` parameter (default `TRUE`).
When `ignoreFail=FALSE` and the HTTP status was not 2xx, it called `stop()`.

**After:** `authenticatedREST()` **always returns `NULL` on failure** and never
calls `stop()`. The `ignoreFail` parameter has been removed.

**Migration:** If your code passed `ignoreFail=TRUE` or `ignoreFail=FALSE`,
remove that argument. The function signature is now:

```r
authenticatedREST(url, urlParams, queryParams, data, restType, contentType, encode)
```

### 2. Non-NULL result guarantees HTTP 2xx

**Before:** A non-NULL result from `authenticatedREST()` could still have a
non-2xx status code, requiring manual status code checks:

```r
result <- authenticatedREST(...)
if (!is.null(result) && result$status_code >= 200 && result$status_code < 300) {
  # success
}
```

**After:** If `authenticatedREST()` returns non-NULL, it is **guaranteed to be
a 2xx response**. All non-2xx responses return NULL.

**Migration:** Simplify status code checks:

```r
# Before
if (!is.null(result) && result$status_code >= 200 && result$status_code < 300) {
  return(TRUE)
}
return(FALSE)

# After
if (!is.null(result)) {
  return(TRUE)
}
return(FALSE)
```

### 3. `lastRestError()` provides failure details

**New feature:** When `authenticatedREST()` returns NULL, the error details are
stored in a package-level variable accessible via `lastRestError()`.

```r
result <- authenticatedREST(...)
if (is.null(result)) {
  err <- lastRestError()
  # err$status_code  - HTTP status code (e.g. 401, 404, 500)
  # err$url          - The full URL that was called
  # err$method       - The HTTP method (GET, POST, PUT, DELETE)
  # err$message      - Human-readable error description
  # err$timestamp    - When the error occurred
}
```

`lastRestError()` returns `NULL` if the last REST call succeeded. Since improveR
is single-threaded, this is safe to use immediately after any REST call.

**Migration:** If your code needs to distinguish failure types (e.g. 401 vs 404),
use `lastRestError()` instead of checking `result$status_code`:

```r
# Before
result <- authenticatedREST(...)
if (!is.null(result) && result$status_code == 200) {
  # success
} else if (!is.null(result) && result$status_code == 404) {
  # not found
}

# After
result <- authenticatedREST(...)
if (!is.null(result)) {
  # success (any 2xx)
} else {
  err <- lastRestError()
  if (!is.null(err) && err$status_code == 404) {
    # not found
  }
}
```

### 4. All logging uses `log_*()` wrappers

**Before:** Mixed usage of direct `logging::logdebug()`, `logging::loginfo()`,
`logging::logwarn()`, `logging::logerror()` and wrapper functions `log_debug()`,
`log_info()`, `log_warn()`, `log_error()`.

**After:** All code uses the `log_*()` wrappers exclusively. The wrappers
support `redirectLogs()` for structured test output.

**Migration:** Replace any direct `logging::` calls in your code:

```r
# Before
logging::loginfo("message")
logging::logwarn("warning")

# After
log_info("message")
log_warn("warning")
```

### 5. Guard clauses always log before returning

**Before:** Some functions returned `NULL` or `FALSE` silently on failure.

**After:** Every early return in exported functions logs a `log_warn()` message
explaining why the operation failed.

**Migration:** If your code suppresses warnings (e.g. `suppressWarnings()`),
be aware that there may be more log output from failed operations. This is
informational and does not change the return values.

### 6. Redundant `tryCatch` blocks removed

**Before:** Many functions wrapped `authenticatedREST()` or `loadResource()`
calls in `tryCatch` blocks that returned NULL on error.

**After:** These redundant `tryCatch` blocks have been removed because:
- `authenticatedREST()` never throws (always returns NULL on failure)
- `loadResource()` handles its own errors internally

**Migration:** If your code wraps improveR calls in `tryCatch`:
- This still works (no breaking change)
- But the error handler will never fire for REST failures since they
  return NULL instead of throwing
- Consider simplifying to NULL checks instead

## Quick Reference: Return Value Contracts

| Operation Type | Success Return | Failure Return |
|---|---|---|
| Load/Get (read) | Data frame or list | `NULL` |
| Create (write) | Data frame (resource) | `NULL` |
| Delete | `TRUE` | `FALSE` |
| Lifecycle (finish/reopen/accept/decline) | `TRUE` | `FALSE` |
| Lock/Unlock | `TRUE` | `FALSE` |

## Quick Reference: Error Handling Pattern

```r
# Standard pattern for all improveR REST wrapper functions:
myFunction <- function(ident, from = pwd()) {
  improveConnected()  # or improveEditable() for write operations

  # Guard: resolve identifier
  resource <- loadResource(ident, from)
  if (is.null(resource)) {
    log_warn("cannot find resource by ident:", ident)
    return(NULL)  # or return(FALSE) for boolean-return functions
  }

  # REST call
  result <- authenticatedREST("/endpoint/{id}",
                              urlParams = list(id = resource$resourceId),
                              restType = "GET")

  # Handle failure (non-NULL = guaranteed 2xx)
  if (is.null(result)) {
    log_warn("failed to perform operation on:", ident)
    return(NULL)  # or return(FALSE)
  }

  # Process success response
  cont <- httr::content(result)
  return(cont)
}
```

## Breaking Changes Summary

| Change | Impact | Action Required |
|---|---|---|
| `ignoreFail` parameter removed | Code passing `ignoreFail=TRUE/FALSE` will get "unused argument" error | Remove the argument |
| Non-NULL = 2xx guarantee | Status code checks are redundant but harmless | Optional: simplify |
| `lastRestError()` added | New exported function | No action (additive) |
| `logging::` → `log_*()` | Internal change only | Only if you call `logging::` directly on improveR internals |
| Silent guards now log | More log output on failures | No action (informational) |

Only the first item (`ignoreFail` removal) is a **hard breaking change** that
requires code modification. All other changes are backward-compatible.
