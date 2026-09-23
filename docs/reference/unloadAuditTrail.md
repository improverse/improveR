# Unload Audit Trail from Cache

Removes audit trail data from the cache for the specified resource.

## Usage

``` r
unloadAuditTrail(ident)
```

## Arguments

- ident:

  The resource identifier - can be a resource ID, entity ID, entity
  version ID, or path.

## Value

Invisibly returns NULL. Called for side effect of clearing cache.

## Details

Use this function when you need to ensure fresh audit trail data is
loaded from the server on the next
[`loadAuditTrail`](https://improverse.github.io/improveR/reference/loadAuditTrail.md)
call.

## References

ics1097

## See also

[`loadAuditTrail`](https://improverse.github.io/improveR/reference/loadAuditTrail.md)
to load audit trail,
[`refreshAuditTrail`](https://improverse.github.io/improveR/reference/refreshAuditTrail.md)
to refresh from server
