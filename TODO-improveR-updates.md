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
- [x] Cache invalidation tests (`test-cacheInvalidation.R`) — 22 tests covering lock/unlock, finish/reopen, create/delete children, push
- [x] Upfront tool validation (`validateWorkflowTools()`) — validates all tool configs exist before import
- [x] Link mapping hash validation — `validateMappingFiles()` compares filehash, warns on mismatch

## 1. Rename Cache-Refresh Functions (`update*` → `refresh*`)

- [x] All 28 cache-refresh functions renamed to `refresh*`
- [x] Deprecated wrappers added for all old `update*` names
- [x] All internal callers updated to use new names
- [x] NAMESPACE updated with new exports
- [x] Man pages updated

### Server-mutation functions (keep `update*` — correct semantic):
- `updateFileContent()` — change.R (uploads new file content)
- `updateMetaDate()` / `updateMetaDateById()` — metadata.R (modifies metadata value)
- `updateLinks()` — updateLinks.R (refreshes outdated links on server)
- `updateResourceRelation()` — resourceRelations.R (modifies a relation)
- `updateGridArgument()` — setGridArguments.R (modifies grid arg value)
- `updateResourcePermission()` — permissions.R (modifies ACL entry)
- `updateAccessToken()` — tokenManagement.R (updates local env vars, not server)

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
- [x] Create tests for cache invalidation scenarios (test-cacheInvalidation.R)
- [ ] Verify multi-user scenarios (user A locks, user B's cache)
- [ ] Test push/pull cycles with cache states

## 5. Cross-Repository Import Issues (found via test-crossRepoImport.R)

### clearConnectionData must reset cliEnv$userProfile
- [x] Mitigated: hash-based profiles (`improveR_<md5>`) mean each server URL gets its
  own profile name, so cross-server conflicts no longer occur.
- [x] `clearConnectionData()` now resets `cliEnv$userProfile <- NULL`

### CLI user profile persists on disk — must reset and re-check on connect
- [x] Fixed: `configureUserProfile()` now generates profile names from `openssl::md5(apiURL)`,
  so each server URL gets a unique on-disk profile. No more stale URL conflicts.

### Upfront tool validation when no ToolMapping is present
- [x] `validateWorkflowTools()` in `exportImportUtils.R` — validates all runserver/tool/instance
  combos exist on target server before import begins. Called from both `importWorkflow.R`
  and `importFolder.R` after `applyToolMappingFromFile()`.

### Link mapping validation should check file hash identity
- [x] `validateMappingFiles()` now compares `filehash` from LinkMapping against actual
  `resource$fileHash` on target. Emits warning on mismatch (not error).

### Folder/tree export: recursively download data and export trees
- [x] `exportFolder()` — recursive folder export with multi-tree unified workflow
- [x] `importFolder()` — recursive folder import with folder structure, trees, files, links
- [x] Shared helpers extracted to `exportImportUtils.R`
- [x] Cross-repo round-trip test (`test-crossRepoFolderImport.R`) — 16/16 pass

### importFolder: behavior when target already exists
- [x] Implemented `onConflict` parameter: `"skip"` (default), `"overwrite"`, `"error"`
- [x] `"skip"`: reuses existing folders/trees, skips file content update
- [x] `"overwrite"`: reuses folders/trees, updates file content via `updateFileContent()`
- [x] `"error"`: upfront conflict detection, stops with list of conflicts before changes
- [x] Old `overwrite=TRUE` boolean mapped to `onConflict="overwrite"` (deprecated)
- [x] Tests in `test-importOnConflict.R` (OC1-OC8)
- Note: tree step duplication on re-import with "skip" is documented (OC7) —
  `realise()` creates new steps inside existing trees. Use "error" to prevent.

### improveConnect should validate connection when token already exists
- [x] `improveConnect()` now validates the token with a lightweight API call after
  config is loaded. If validation fails: OAuth → clears stale data and re-authenticates;
  run token → errors with clear message (or falls back to offline mode if `offlinePossible=TRUE`).

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