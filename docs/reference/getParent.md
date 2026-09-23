# Returns the Parent of Anything Resolved by GetCorrectId

Returns the Parent of Anything Resolved by GetCorrectId

## Usage

``` r
getParent(identifier)
```

## Arguments

- identifier:

  ID or resource

## Value

The `parentId` of the resource, and `0` when the resource has no parent.
Note that `0` is also what a resource that could not be loaded yields,
so the value does not distinguish the root from a failed read.

## References

ics1088
