# TODO: improveR Updates

## Completed

- Add repository version check in improveConnect
- Create checkConnect function to verify connection validity
- Add getRepositoryVersion function
- CLI hash-based user profiles (no cross-server conflicts)
- CLI token auth: use `renewAccessToken()` in
  cloneCli/pushCli/pushRunCli/pullCli
- Cache invalidation: pushCli/pushRunCli/pullCli now invalidate
  resource + child caches
- Cache invalidation: lockResource/unlockResource now unload resource
  cache after state change
- Cache invalidation: finishResource/reopenResource now unload resource
  cache after state change
- Empty API result handling: actualLoad\* functions return data.frame()
  instead of NULL
- Cache invalidation tests (`test-cacheInvalidation.R`) — 22 tests
  covering lock/unlock, finish/reopen, create/delete children, push
- Upfront tool validation
  ([`validateWorkflowTools()`](https://improverse.github.io/improveR/reference/validateWorkflowTools.md))
  — validates all tool configs exist before import
- Link mapping hash validation —
  [`validateMappingFiles()`](https://improverse.github.io/improveR/reference/validateMappingFiles.md)
  compares filehash, warns on mismatch

## 1. Rename Cache-Refresh Functions (`update*` → `refresh*`)

- All 28 cache-refresh functions renamed to `refresh*`
- Deprecated wrappers added for all old `update*` names
- All internal callers updated to use new names
- NAMESPACE updated with new exports
- Man pages updated

### Server-mutation functions (keep `update*` — correct semantic):

- [`updateFileContent()`](https://improverse.github.io/improveR/reference/updateFileContent.md)
  — change.R (uploads new file content)
- [`updateMetaDate()`](https://improverse.github.io/improveR/reference/updateMetaDate.md)
  /
  [`updateMetaDateById()`](https://improverse.github.io/improveR/reference/updateMetaDateById.md)
  — metadata.R (modifies metadata value)
- [`updateLinks()`](https://improverse.github.io/improveR/reference/updateLinks.md)
  — updateLinks.R (refreshes outdated links on server)
- [`updateResourceRelation()`](https://improverse.github.io/improveR/reference/updateResourceRelation.md)
  — resourceRelations.R (modifies a relation)
- `updateGridArgument()` — setGridArguments.R (modifies grid arg value)
- [`updateResourcePermission()`](https://improverse.github.io/improveR/reference/updateResourcePermission.md)
  — permissions.R (modifies ACL entry)
- [`updateAccessToken()`](https://improverse.github.io/improveR/reference/updateAccessToken.md)
  — tokenManagement.R (updates local env vars, not server)

## 2. Cache Invalidation Review

The following functions need cache invalidation review:

### updateLink

- `updateLinks` already invalidates link resource + parent child cache
  (verified)

### push (cliPush)

- `pushCli` and `pushRunCli` now invalidate resource + child resource
  caches after push

### lock/unlock

- `lockResource` and `unlockResource` now unload resource cache after
  state change
- Note: child resources don’t need cache updates (lock is per-resource,
  not inherited)

## 3. Additional CLI Function Reviews

- `pullCli` now invalidates resource + child resource caches after pull
- `cloneCli` is read-only (downloads to local) — no cache invalidation
  needed
- `delete` already has comprehensive cache invalidation
  (invalidatePathCaches + unloadChildResources)

## 4. Testing Requirements

- Create tests for cache invalidation scenarios
  (test-cacheInvalidation.R)
- Verify multi-user scenarios (user A locks, user B’s cache)
- Test push/pull cycles with cache states

## 5. Cross-Repository Import Issues (found via test-crossRepoImport.R)

### clearConnectionData must reset cliEnv\$userProfile - \[x\] Mitigated: hash-based profiles (\`improveR\_\<md5\>\`) mean each server URL gets its own profile name, so cross-server conflicts no longer occur. - \[x\] \`clearConnectionData()\` now resets \`cliEnv\$userProfile \<- NULL\`

### CLI user profile persists on disk — must reset and re-check on connect

- Fixed: `configureUserProfile()` now generates profile names from
  `openssl::md5(apiURL)`, so each server URL gets a unique on-disk
  profile. No more stale URL conflicts.

### Upfront tool validation when no ToolMapping is present

- [`validateWorkflowTools()`](https://improverse.github.io/improveR/reference/validateWorkflowTools.md)
  in `exportImportUtils.R` — validates all runserver/tool/instance
  combos exist on target server before import begins. Called from both
  `importWorkflow.R` and `importFolder.R` after
  [`applyToolMappingFromFile()`](https://improverse.github.io/improveR/reference/applyToolMappingFromFile.md).

### Link mapping validation should check file hash identity

- [`validateMappingFiles()`](https://improverse.github.io/improveR/reference/validateMappingFiles.md)
  now compares `filehash` from LinkMapping against actual
  `resource$fileHash` on target. Emits warning on mismatch (not error).

### Folder/tree export: recursively download data and export trees

- [`exportFolder()`](https://improverse.github.io/improveR/reference/exportFolder.md)
  — recursive folder export with multi-tree unified workflow
- [`importFolder()`](https://improverse.github.io/improveR/reference/importFolder.md)
  — recursive folder import with folder structure, trees, files, links
- Shared helpers extracted to `exportImportUtils.R`
- Cross-repo round-trip test (`test-crossRepoFolderImport.R`) — 16/16
  pass

### importFolder: behavior when target already exists

- Implemented `onConflict` parameter: `"skip"` (default), `"overwrite"`,
  `"error"`
- `"skip"`: reuses existing folders/trees, skips file content update
- `"overwrite"`: reuses folders/trees, updates file content via
  [`updateFileContent()`](https://improverse.github.io/improveR/reference/updateFileContent.md)
- `"error"`: upfront conflict detection, stops with list of conflicts
  before changes
- Old `overwrite=TRUE` boolean mapped to `onConflict="overwrite"`
  (deprecated)
- Tests in `test-importOnConflict.R` (OC1-OC8)
- Note: tree step duplication on re-import with “skip” is documented
  (OC7) — `realise()` creates new steps inside existing trees. Use
  “error” to prevent.

### improveConnect should validate connection when token already exists

- [`improveConnect()`](https://improverse.github.io/improveR/reference/improveConnect.md)
  now validates the token with a lightweight API call after config is
  loaded. If validation fails: OAuth → clears stale data and
  re-authenticates; run token → errors with clear message (or falls back
  to offline mode if `offlinePossible=TRUE`).

### CLI JAR rejects refreshed tokens after ~20 minutes (Keycloak SSO session?)

- Fixed: CLI functions now call `renewAccessToken()` before each CLI
  invocation, ensuring the JAR always receives a fresh token. This
  bypasses the Keycloak SSO session timeout issue entirely.
- Root cause (Keycloak SSO session expiry) is not fully understood but
  is no longer a practical problem. The following notes are preserved
  for reference:
- The CLI JAR (`clone`/`push`/`pushRun`) accepts `-accessToken` and uses
  it for auth. Fresh tokens from the initial OAuth device flow work. But
  after a certain time (~20 min), refreshed tokens start failing with
  “Authorization failed for improve server” even though the tokens are
  structurally valid (correct iss, aud, azp, non-expired).
- Investigation findings:
  - Token lifetime is 300 seconds (5 min), refreshed every ~100 seconds
    by shared token manager
  - First 10 clones succeed (using tokens from the first ~8 minutes
    after `auth_time`)
  - All subsequent clones fail (~81 of them), even with freshly
    refreshed tokens
  - Working vs failing tokens are identical in structure (same `iss`,
    `aud`, `azp`, `sid`, `scope`)
  - The only difference is `iat`/`exp` — failing tokens have later
    timestamps
  - The `auth_time` stays the same (from original OAuth flow),
    suggesting the Keycloak SSO session may be expiring and the server
    rejects tokens whose session is invalidated
  - In isolation (fresh R session), both initial and refreshed tokens
    work fine — the issue only manifests during long-running test suites
- Note: `authenticatedREST` also calls
  [`refreshToken()`](https://improverse.github.io/improveR/reference/refreshToken.md)
  before each API call, which may interact with the CLI’s token usage.
  The shared token refresher runs in a background process and writes
  tokens to a file; the main process reads them via
  [`applyTokenData()`](https://improverse.github.io/improveR/reference/applyTokenData.md).
- Possible causes: Keycloak SSO session timeout, stale session cookie in
  the JAR’s profile, or a race condition between the background
  refresher and the main process’s token state.
- Also: the `configureUserProfile` does not pass
  `-checkCertificates false` unless `secure=FALSE` is explicitly set.
  The `improveRtestsupport::improveConnect()` defaults to `secure=TRUE`.
  The test runner calls it without `secure=FALSE`. This should be made
  consistent.
- **Files**: `improveR/R/cliClone.R`, `improveR/R/tokenManagement.R`,
  `improveR/R/cliSetup.R`

### CLI argument handling is fragile

- The CLI command construction uses
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) to
  build command strings by string interpolation
  (e.g. `"clone -accessToken {accessToken} -resource {resource$entityId}"`).
  This is fragile — if any argument contains spaces, special characters,
  or is NULL/NA, the command silently breaks or produces unexpected
  behavior.
- Consider using [`system2()`](https://rdrr.io/r/base/system2.html) with
  proper argument vector instead of
  [`system()`](https://rdrr.io/r/base/system.html) with pasted strings,
  or at minimum quote/escape arguments.
- **File**: `improveR/R/cliExecute.R`, `improveR/R/cliClone.R`,
  `improveR/R/cliPush.R`, `improveR/R/cliPushRun.R`,
  `improveR/R/cliSetup.R`

## Notes

- Cache invalidation is critical for data consistency
- Consider implementing a cache invalidation strategy document
- May need to add cache debugging functions for troubleshooting
