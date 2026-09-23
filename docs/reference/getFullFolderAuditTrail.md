# Retrieve Recursive Audit Trail For A Specific Folder

Retrieve Recursive Audit Trail For A Specific Folder

## Usage

``` r
getFullFolderAuditTrail(ident, from = pwd(), includeReadAccess = FALSE)
```

## Arguments

- ident:

  the folder the audit trail is retrieved for

- from:

  startpoint for relative paths, per default: pwd is used

- includeReadAccess:

  if TRUE also read access entries of the audit trail are shown

## Value

A data frame of audit trail entries for the folder and everything below
it, made distinct over revision, entity, description and timestamp (and
over the changed attribute where the entries carry one), ordered by
`createdAt`. Read access entries are only included with
`includeReadAccess = TRUE`. This call bypasses the audit trail cache by
design.
