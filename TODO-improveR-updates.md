# TODO: improveR Updates

## Completed
- [x] Add repository version check in improveConnect
- [x] Create checkConnect function to verify connection validity
- [x] Add getRepositoryVersion function
- [x] CLI hash-based user profiles (no cross-server conflicts)
- [x] CLI token auth: use `renewAccessToken()` in cloneCli/pushCli/pushRunCli/pullCli
- [x] Cache invalidation: pushCli/pushRunCli/pullCli now invalidate resource + child caches
- [x] Cache invalidation: lockResource/unlockResource now unload resource cache after state change
- [x] Cache invalidation: finishResource/reopenResource now unload resource cache after state change
- [x] Empty API result handling: actualLoad* functions return data.frame() instead of NULL

## 1. updateLinks Function
- [ ] Update `updateLinks` function to have consistent syntax with other update functions
- [ ] Should follow pattern similar to `updateFileContent`, `updateMetadata`, etc.
- [ ] Current syntax may be inconsistent with other API calls

## 2. Cache Invalidation Review
The following functions need cache invalidation review:

### updateLink
- [x] `updateLinks` already invalidates link resource + parent child cache (verified)

### push (cliPush)
- [x] `pushCli` and `pushRunCli` now invalidate resource + child resource caches after push

### lock/unlock
- [x] `lockResource` and `unlockResource` now unload resource cache after state change
- Note: child resources don't need cache updates (lock is per-resource, not inherited)

## 3. Additional CLI Function Reviews
- [x] `pullCli` now invalidates resource + child resource caches after pull
- [x] `cloneCli` is read-only (downloads to local) — no cache invalidation needed
- [x] `delete` already has comprehensive cache invalidation (invalidatePathCaches + unloadChildResources)

## 4. Testing Requirements
- [ ] Create tests for cache invalidation scenarios
- [ ] Verify multi-user scenarios (user A locks, user B's cache)
- [ ] Test push/pull cycles with cache states

## 5. Cross-Repository Import Issues (found via test-crossRepoImport.R)

### clearConnectionData must reset cliEnv$userProfile
- [x] Mitigated: hash-based profiles (`improveR_<md5>`) mean each server URL gets its
  own profile name, so cross-server conflicts no longer occur.
- [ ] Nice-to-have: `clearConnectionData()` could still reset `cliEnv$userProfile <- NULL`
  for correctness, but it's no longer causing auth failures.

### CLI user profile persists on disk — must reset and re-check on connect
- [x] Fixed: `configureUserProfile()` now generates profile names from `openssl::md5(apiURL)`,
  so each server URL gets a unique on-disk profile. No more stale URL conflicts.

### Upfront tool validation when no ToolMapping is present
- [ ] When no ToolMapping.json file exists, validate before import that all tools
  referenced in the workflow actually exist on the target server.
- Currently this is only discovered at `realise()` time per step, which means partial
  imports can occur (some steps created, then failure mid-way through the workflow).
- The validation should extract all tool keys from the workflow steps, check them against
  `getToolInstances()`, and abort early with a clear error listing missing tools — the
  same way filled tool mappings are validated in `validateImportMappings()`.
- **File**: `improveR/R/importWorkflow.R`, around the `else` branch at "No tool mapping file found"

### Link mapping validation should check file hash identity
- [ ] Link mapping validation (`validateImportMappings`) only checks that the mapped
  resource exists on the target server (`loadResource(ident)`), but does NOT compare
  the `filehash` from the mapping against the actual file's hash on the target.
- The LinkMapping.json contains a `filehash` column from the export. During validation,
  the loaded resource's hash should be compared against the expected hash to ensure the
  user mapped to the correct file (not just any file that happens to exist).
- A mismatch should produce a warning (not an error), since the user may intentionally
  map to a different version of the file.
- **File**: `improveR/R/importWorkflow.R`, function `validateImportMappings()`, link validation loop

### Folder/tree export: recursively download data and export trees
- [ ] Implement recursive folder export — download all data from a folder tree
  and package it for transfer (not just workflow steps, but arbitrary folder hierarchies).
- [ ] Corresponding recursive folder import — recreate folder structure and upload
  all data into a target location.
- [ ] Handle tree (analysis tree) export/import as part of this, preserving the
  tree structure and step relationships.

### folderUpload: behavior when target already exists
- [ ] Clarify and test what happens when `uploadFolder` is called and the target
  folder (or files within it) already exists on the server.
- Should it overwrite, skip, error, or create new versions?
- Current behavior needs to be documented and tested for edge cases.

### improveConnect should validate connection when token already exists
- [ ] If `improveConnect()` is called and a token is already present (e.g. from a
  previous session or stale env vars), it should run `checkConnect()` to verify the
  connection is actually valid before returning success.
- Currently, calling `improveConnect()` can silently succeed (token exists, no error
  thrown) even though the connection is dead. Calling it again produces the same
  result — the user is never actually connected and gets no feedback.
- `improveConnect()` should: detect existing token → call `checkConnect()` → if
  `checkConnect()` fails, clear stale token data and re-authenticate from scratch.
- This prevents the "call connect, not connected, call again, still not connected"
  loop that users hit when tokens are expired or the server has changed.
- **File**: `improveR/R/improveConnect.R`

### CLI JAR rejects refreshed tokens after ~20 minutes (Keycloak SSO session?)
- [x] Fixed: CLI functions now call `renewAccessToken()` before each CLI invocation,
  ensuring the JAR always receives a fresh token. This bypasses the Keycloak SSO
  session timeout issue entirely.
- [ ] Root cause (Keycloak SSO session expiry) is not fully understood but is no longer
  a practical problem. The following notes are preserved for reference:
- [ ] The CLI JAR (`clone`/`push`/`pushRun`) accepts `-accessToken` and uses it for auth.
  Fresh tokens from the initial OAuth device flow work. But after a certain time (~20 min),
  refreshed tokens start failing with "Authorization failed for improve server" even though
  the tokens are structurally valid (correct iss, aud, azp, non-expired).
- Investigation findings:
  - Token lifetime is 300 seconds (5 min), refreshed every ~100 seconds by shared token manager
  - First 10 clones succeed (using tokens from the first ~8 minutes after `auth_time`)
  - All subsequent clones fail (~81 of them), even with freshly refreshed tokens
  - Working vs failing tokens are identical in structure (same `iss`, `aud`, `azp`, `sid`, `scope`)
  - The only difference is `iat`/`exp` — failing tokens have later timestamps
  - The `auth_time` stays the same (from original OAuth flow), suggesting the Keycloak SSO
    session may be expiring and the server rejects tokens whose session is invalidated
  - In isolation (fresh R session), both initial and refreshed tokens work fine —
    the issue only manifests during long-running test suites
- Note: `authenticatedREST` also calls `refreshToken()` before each API call, which may
  interact with the CLI's token usage. The shared token refresher runs in a background
  process and writes tokens to a file; the main process reads them via `applyTokenData()`.
- Possible causes: Keycloak SSO session timeout, stale session cookie in the JAR's profile,
  or a race condition between the background refresher and the main process's token state.
- Also: the `configureUserProfile` does not pass `-checkCertificates false` unless `secure=FALSE`
  is explicitly set. The `improveRtestsupport::improveConnect()` defaults to `secure=TRUE`.
  The test runner calls it without `secure=FALSE`. This should be made consistent.
- **Files**: `improveR/R/cliClone.R`, `improveR/R/tokenManagement.R`, `improveR/R/cliSetup.R`

### CLI argument handling is fragile
- [ ] The CLI command construction uses `glue::glue()` to build command strings by
  string interpolation (e.g. `"clone -accessToken {accessToken} -resource {resource$entityId}"`).
  This is fragile — if any argument contains spaces, special characters, or is NULL/NA,
  the command silently breaks or produces unexpected behavior.
- [ ] Consider using `system2()` with proper argument vector instead of `system()` with
  pasted strings, or at minimum quote/escape arguments.
- **File**: `improveR/R/cliExecute.R`, `improveR/R/cliClone.R`, `improveR/R/cliPush.R`,
  `improveR/R/cliPushRun.R`, `improveR/R/cliSetup.R`

## Notes
- Cache invalidation is critical for data consistency
- Consider implementing a cache invalidation strategy document
- May need to add cache debugging functions for troubleshooting