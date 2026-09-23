# checkConnect

Makes sure the session is usable: checks whether the connection is still
valid by loading the `IMPROVER_STEP` resource, and rebuilds the
connection if it is not. It always recovers - there is no way to ask for
a check without one.

## Usage

``` r
checkConnect(secure = TRUE)
```

## Arguments

- secure:

  Logical, passed on to
  [`improveConnect()`](https://improverse.github.io/improveR/reference/improveConnect.md)
  when a connection has to be established or rebuilt. It is the TLS
  flag: `FALSE` turns off certificate verification. It does **not**
  control whether recovery is attempted. The previous documentation said
  it did, which meant a caller who wanted a check without recovery was
  told to switch certificate checking off instead (IMR-290).

## Value

`TRUE`, invisibly - **always**. Every path through the function returns
it: the connection was valid, the connection was rebuilt, or there was
no connection to begin with and one was made. A caller cannot tell those
apart from the return value, and cannot learn from it that anything
failed; a failure to reconnect surfaces as an error from
[`improveConnect()`](https://improverse.github.io/improveR/reference/improveConnect.md)
instead. Whether this function should be able to answer `FALSE` is an
open question (IMR-290), not an oversight in this description.

## See also

[`improveConnect()`](https://improverse.github.io/improveR/reference/improveConnect.md),
[`improveConnected()`](https://improverse.github.io/improveR/reference/improveConnected.md)
