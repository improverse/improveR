# Get the Repository Entity ID Prefix

Returns the prefix used for entity IDs on the connected repository (e.g.
`"hc4310:"`). When no step ID is available, the prefix is
auto-discovered during
[`improveConnect`](https://improverse.github.io/improveR/reference/improveConnect.md).

## Usage

``` r
repoPrefix()
```

## Value

Character string with the prefix (including trailing colon), or `""` if
not available.
