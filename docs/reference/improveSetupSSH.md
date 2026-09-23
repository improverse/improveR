# Set Up SSH Keys for Tunnel Access

Generates an EC P-256 key pair for SSH tunnel authentication. The key is
stored at `~/.improve/ssh/client_key.pem`. If a key already exists, it
is not overwritten.

## Usage

``` r
improveSetupSSH(keyDir = NULL)
```

## Arguments

- keyDir:

  Optional. Directory for key storage. Defaults to `~/.improve/ssh/` or
  the `IMPROVE_SSH_KEY_DIR` environment variable.

## Value

The SHA256 fingerprint string (invisibly).

## Details

The fingerprint is printed and returned invisibly. This fingerprint is
automatically registered with the server when using
[`improveOpenTunnel`](https://improverse.github.io/improveR/reference/improveOpenTunnel.md).

Requires `ssh-keygen` (OpenSSH) to be available on the system PATH. On
Windows, this is included with Git for Windows or the built-in OpenSSH
feature.

## Examples

``` r
if (FALSE) { # \dontrun{
improveSetupSSH()
# SHA256:8c/2N0l/ZmFgogfEDU+a74HaeKZK4rrk8VCuiqqcCmQ
} # }
```
