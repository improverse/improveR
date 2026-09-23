# Load Audit Trail

Loads the complete audit trail for a given resource identifier. The
audit trail contains a chronological record of all operations performed
on the resource, supporting regulatory compliance and data integrity
requirements. Results are cached for performance.

## Usage

``` r
loadAuditTrail(ident, from = pwd())
```

## Arguments

- ident:

  The resource identifier - can be a resource ID, entity ID, entity
  version ID, or path. Can also be a list of identifiers.

- from:

  Reference point for relative paths. Defaults to current working
  directory via
  [`pwd`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A data frame with class "improveResource" containing:

- `type` - Type indicator ("auditTrail")

- `resourceId`, `entityId`, `entityVersionId` - Resource identifiers

- `path`, `name` - Resource location and name

- `data` - A list containing data frames with audit trail entries. Each
  entry contains 13 variables documenting a single operation:

  - `id` - Unique audit entry identifier

  - `ipAddress` - IP address of user performing the action

  - `createdAt`, `createdAtDate` - Timestamp (milliseconds and POSIXct)

  - `actor` - Actor type (e.g., "User", "System")

  - `operation` - Operation type (e.g., "create", "update", "delete")

  - `userVersion` - User version identifier

  - `username` - Username who performed the action

  - `description` - Human-readable description of the action

  - `path` - Resource path at time of action

  - `resourceName` - Name of the resource

  - `entityReference` - Reference to the modified entity

  - `entityReferenceType` - Type of entity (e.g., "Step Version",
    "Analysis Tree Version")

  - `revisionId` - Revision identifier

  - `entityId`, `entityVersionId` - Entity identifiers

## Details

The audit trail provides a complete, immutable record of all changes to
a resource, supporting regulatory compliance requirements. Each entry
captures who made the change, when, from where (IP address), and what
was changed.

Audit trails are essential for pharmaceutical workflows to demonstrate
data integrity and provide accountability for all modifications to
analysis results and workflows.

## References

ics1097

## See also

[`unloadAuditTrail`](https://improverse.github.io/improveR/reference/unloadAuditTrail.md)
to clear cache,
[`refreshAuditTrail`](https://improverse.github.io/improveR/reference/refreshAuditTrail.md)
to refresh from server

## Examples

``` r
if (FALSE) { # \dontrun{
# Load audit trail for current resource
auditTrail <- loadAuditTrail(ident, pwd())
auditTrail$data[[1]]

# Review recent operations
recent <- auditTrail$data[[1]] |>
  dplyr::arrange(desc(createdAtDate)) |>
  head(10)

# Load audit trail by path
auditTrail <- loadAuditTrail("/Projects/MyWorkflow/Step1")
} # }
```
