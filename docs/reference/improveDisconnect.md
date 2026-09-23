# improveDisconnect

improveDisconnect removes all connection information.

## Usage

``` r
improveDisconnect(env = cacheEnv)
```

## Arguments

- env:

  default is 'cacheEnv'

## Value

No meaningful value - called for its side effect of emptying `env`. The
authentication provider and the `editable` flag are the only entries
that survive; everything else, the caches included, is removed.

## See also

[`improveConnect()`](https://improverse.github.io/improveR/reference/improveConnect.md),
[`improveConnected()`](https://improverse.github.io/improveR/reference/improveConnected.md)
