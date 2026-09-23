# improveConnected

improveConnected checks if improveConnect was called.

## Usage

``` r
improveConnected(silent = FALSE)
```

## Arguments

- silent:

  if TRUE no log message is printed

## Value

`TRUE` when
[`improveConnect()`](https://improverse.github.io/improveR/reference/improveConnect.md)
has run in this session, `FALSE` otherwise - invisibly in both cases.
With `silent = FALSE` (the default) the unconnected case does not return
at all but stops with `not connected`; only `silent = TRUE` makes this a
predicate.

## See also

[`improveConnect()`](https://improverse.github.io/improveR/reference/improveConnect.md),
[`improveDisconnect()`](https://improverse.github.io/improveR/reference/improveDisconnect.md)
