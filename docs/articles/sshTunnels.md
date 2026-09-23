# SSH Tunnels to Interactive Steps

## Overview

When a step runs an interactive tool like RStudio or Jupyter, improve
exposes it on a Docker port behind the runserver firewall.
[`improveOpenTunnel()`](https://improverse.github.io/improveR/reference/improveOpenTunnel.md)
creates an SSH tunnel so you can access it from your browser via
`localhost`.

## One-time setup

Generate an SSH key pair. This only needs to be done once per machine.

``` r
library(improveR)

improveSetupSSH()
# SSH fingerprint: SHA256:xCwizvT982RCRQmM6d+x9QZYcfU/B42MU76ZwaMGCX8
```

The key is stored in your improve internal directory. The fingerprint is
registered automatically with the server when you open a tunnel.

## Connect to an interactive step

Create a step with an interactive tool (e.g. RStudio), then open the
tunnel:

``` r
# Create a step with RStudio
stepEnv <- createStepTemplateEnv(treeIdent = myTree)
stepEnv$setStepRunserverLabel("runserver")
stepEnv$setStepToolLabel("R 3_Std")
stepEnv$setStepToolInstance("imRstudio 1.3")
stepEnv$setStepDescription("Interactive Analysis")
step <- stepEnv$realise(run = FALSE)

# Open tunnel — starts the step, opens your browser
tunnel <- improveOpenTunnel(step, localPort = 8787)
```

This:

1.  Registers your SSH key with the run
2.  Starts the step on the runserver
3.  Waits for it to reach RUNNING status
4.  Reads the dynamically assigned Docker port
5.  Opens an SSH tunnel from `localhost:8787` to the container
6.  Opens your browser to the RStudio session

## Working with the tunnel

``` r
# Check if tunnel is still alive
tunnel$isAlive()

# Get the URL
tunnel$url
# "http://A1B2C3D4.localhost:8787/"

# Close when done
improveCloseTunnel(tunnel)
```

The tunnel is automatically closed when your R session ends.

## Requirements

- `ssh` and `ssh-keygen` on your system PATH (included with OpenSSH on
  all platforms)
- `processx` R package (optional, improves SSH process management)
- The runserver must have an SSH port configured
