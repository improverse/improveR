# Open SSH Tunnel to a Running Step

Opens an SSH port-forwarding tunnel to an interactive step (e.g.
RStudio, Jupyter) running on an improve runserver. If the step is not
yet running, it is started with the SSH public key fingerprint
registered for authentication.

## Usage

``` r
improveOpenTunnel(ident, localPort = 8787, browse = TRUE)
```

## Arguments

- ident:

  A step identifier. Can be a step path, resource ID, entity ID, or a
  step environment returned by `realise()`.

- localPort:

  Integer. The local port to bind. Defaults to 8787 (RStudio default).

- browse:

  Logical. If `TRUE` (default), opens the tunnel URL in the default
  browser.

## Value

An environment (tunnel object) with:

- `url`:

  The URL to access the step (e.g. `http://<runId>.localhost:8787/`)

- `isAlive()`:

  Returns `TRUE` if the tunnel is still open

- [`close()`](https://rdrr.io/r/base/connections.html):

  Closes the tunnel

- `pid`:

  The SSH process PID

- `runId`:

  The run ID

- `stepIdent`:

  The step identifier

## Details

The function performs the following steps:

1.  Generates an SSH key pair if one does not exist

2.  If the step is not running, starts it with the SSH fingerprint
    registered

3.  Waits for the step to reach RUNNING status

4.  Reads the dynamically assigned Docker port from run metadata

5.  Opens an SSH tunnel from `localhost:<localPort>` to the container
    port

6.  Optionally opens the browser

The tunnel is automatically closed when the R session ends.

Requires the `processx` package (Suggests) and `ssh`/`ssh-keygen` on the
system PATH.

## See also

[`improveSetupSSH`](https://improverse.github.io/improveR/reference/improveSetupSSH.md),
[`improveCloseTunnel`](https://improverse.github.io/improveR/reference/improveCloseTunnel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
stepEnv <- createStepTemplateEnv(treeIdent = myTree)
stepEnv$setStepRunserverLabel("runserver")
stepEnv$setStepToolLabel("R 3_Std")
stepEnv$setStepToolInstance("imRstudio 1.3")
step <- stepEnv$realise(run = FALSE)

tunnel <- improveOpenTunnel(step, localPort = 8787)
# Opens browser to http://<runId>.localhost:8787/

tunnel$isAlive()
tunnel$close()
} # }
```
