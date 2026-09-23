# Resource Lifecycle

## Overview

Resources in improve can be **finished** (locked) to prevent further
modifications. This is used to protect validated work from accidental
changes. Finishing a folder locks all its contents.

## Finishing a resource

### Finish a single file

``` r
library(improveR)

finishResource(file$resourceId)

# Verify
res <- refreshResource(file$resourceId)
res$finishedStatus
# "finishedInherited"
```

### Finish a folder (locks all children)

``` r
finishResource(folder$resourceId)

# All children are finished too
refreshResource(childFile$resourceId)$finishedStatus
# "finishedInherited"

refreshResource(subFolder$resourceId)$finishedStatus
# "finishedInherited"
```

### What’s blocked

Finished resources cannot be modified:

``` r
# This returns NULL — blocked
createFile(finishedFolder$resourceId, fileName = "new.txt")
```

## Reopening a resource

``` r
reopenResource(folder$resourceId)

# All children are unfinished again
refreshResource(childFile$resourceId)$finishedStatus
# "unfinished"

# Modifications work again
createFile(folder$resourceId, fileName = "new.txt")
```

## Typical workflow

``` r
# 1. Do your analysis work
file <- createFile(folder, localPath = "results.csv", fileName = "results.csv")

# 2. Lock when validated
finishResource(folder$resourceId)

# 3. If revisions are needed, reopen
reopenResource(folder$resourceId)

# 4. Make changes, then re-lock
finishResource(folder$resourceId)
```
