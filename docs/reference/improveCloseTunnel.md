# Close an SSH Tunnel

Closes an SSH tunnel previously opened by
[`improveOpenTunnel`](https://improverse.github.io/improveR/reference/improveOpenTunnel.md).

## Usage

``` r
improveCloseTunnel(tunnel)
```

## Arguments

- tunnel:

  A tunnel object returned by
  [`improveOpenTunnel`](https://improverse.github.io/improveR/reference/improveOpenTunnel.md).

## Value

`NULL`, invisibly. Stops with an error if `tunnel` is not the object
[`improveOpenTunnel()`](https://improverse.github.io/improveR/reference/improveOpenTunnel.md)
returned.

## See also

[`improveOpenTunnel`](https://improverse.github.io/improveR/reference/improveOpenTunnel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
tunnel <- improveOpenTunnel(step)
# ... use RStudio ...
improveCloseTunnel(tunnel)
} # }
```
