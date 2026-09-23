# Refresh Audit Trail from Server

Clears cached audit trail data and reloads fresh data from the server.

## Usage

``` r
refreshAuditTrail(ident)

updateAuditTrail(...)
```

## Arguments

- ident:

  The resource identifier - can be a resource ID, entity ID, entity
  version ID, or path.

- ...:

  For backwards compatibility with the deprecated `update*` alias; not
  used by `refresh*` itself.

## Value

A data frame with updated audit trail data. See
[`loadAuditTrail`](https://improverse.github.io/improveR/reference/loadAuditTrail.md)
for details on the return structure.

## Details

Use this function to ensure you have the most recent audit trail
entries, particularly after operations that may have generated new audit
records.

## References

ics1097

## See also

[`loadAuditTrail`](https://improverse.github.io/improveR/reference/loadAuditTrail.md)
for return structure details,
[`unloadAuditTrail`](https://improverse.github.io/improveR/reference/unloadAuditTrail.md)
to only clear cache
