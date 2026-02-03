# TODO: improveR Updates

## Completed
- [x] Add repository version check in improveConnect
- [x] Create checkConnect function to verify connection validity
- [x] Add getRepositoryVersion function

## 1. updateLinks Function
- [ ] Update `updateLinks` function to have consistent syntax with other update functions
- [ ] Should follow pattern similar to `updateFileContent`, `updateMetadata`, etc.
- [ ] Current syntax may be inconsistent with other API calls

## 2. Cache Invalidation Review
The following functions need cache invalidation review:

### updateLink
- [ ] Check which caches are affected when a link is updated
- [ ] Ensure link cache is invalidated
- [ ] Verify target resource cache handling

### push (cliPush)
- [ ] Review which caches need invalidation after push
- [ ] File cache invalidation
- [ ] Resource cache invalidation
- [ ] Check if child resource caches are affected

### lock/unlock
- [ ] Determine cache invalidation needs for lock operations
- [ ] Resource state cache
- [ ] Permission-related caches
- [ ] Check if child resources need cache updates

## 3. Additional CLI Function Reviews
- [ ] Verify pull (cliPull) cache handling
- [ ] Check clone (cliClone) cache initialization
- [ ] Review delete operations cache cleanup

## 4. Testing Requirements
- [ ] Create tests for cache invalidation scenarios
- [ ] Verify multi-user scenarios (user A locks, user B's cache)
- [ ] Test push/pull cycles with cache states

## Notes
- Cache invalidation is critical for data consistency
- Consider implementing a cache invalidation strategy document
- May need to add cache debugging functions for troubleshooting