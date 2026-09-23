# Create Transaction

Opens a new transaction on the repository, optionally locking a
resource.

## Usage

``` r
createTransaction(comment = "", lockResourceId = NULL)
```

## Arguments

- comment:

  Optional comment for the transaction.

- lockResourceId:

  Optional resource ID to lock within the transaction.

## Value

A list with the created transaction data, or `NULL` on failure.

## References

ccs11
