# improveConnect

improveConnect establishes a connection to the improve repository using
one of two authentication methods:

### Authentication Methods:

1.  **Run Tokens (Production):** When a step is executed from improve
    platform, a run token is automatically provided

2.  **OAuth (Interactive):** User authentication with browser or
    headless mode for development/interactive use

### Required Environment Variables:

**For Run Token Authentication:**

- IMPROVER_TOKEN: the run token (mandatory)

- IMPROVER_REPO_URL: repository URL (mandatory)

- IMPROVER_STEP: step ID for relative path resolution (optional)

- IMPROVER_WORKSPACE: workspace directory (optional, defaults to current
  directory)

**For OAuth Authentication:**

- IMPROVER_REPO_URL: repository URL (mandatory)

- IMPROVER_STEP: step ID for relative path resolution (mandatory)

- IMPROVER_HEADLESS_OAUTH: set to any non-empty value to enable headless
  OAuth mode (optional)

- IMPROVER_WORKSPACE: workspace directory (optional, defaults to current
  directory)

### Optional Environment Variables (both methods):

- IMPROVER_SECURITY: set to "insecure" to disable certificate
  verification

### Connection information sources (in descending priority):

1.  via the command line

2.  via environment variables

3.  via a conf.json file

### Command line

Command line arguments have to be in the correct order.

1.  run token (reqToken)

2.  step ID (shortEntityId) for relative path resolution

3.  the repository URL in the format
    https://repoaddress:repoPort/repository

4.  run workspace path

### Conf.json file

The file has to be located in the working directory and contain the
following details:

- reqToken: run token (if not provided, OAuth authentication will be
  used)

- stepId: step ID for relative path resolution (mandatory for OAuth)

- repoUrl: repository URL (mandatory)

- runWorkspace: workspace directory

## Usage

``` r
improveConnect(
  logLevel = "INFO",
  secure = TRUE,
  offlinePossible = FALSE,
  persistentCaching = FALSE
)
```

## Arguments

- logLevel:

  Log verbosity level. Possible values: DEBUG, INFO, WARN, ERROR.
  Default is "INFO". Can be overridden by environment variable
  IMPROVE_LOG_LEVEL.

- secure:

  If TRUE (default), SSL certificates are validated. If FALSE,
  certificate validation is disabled - this allows connections with
  expired or self-signed certificates but is NOT recommended for
  production use. Can be overridden by setting environment variable
  IMPROVER_SECURITY="insecure".

- offlinePossible:

  If TRUE, the setup continues even if no connection is possible.
  Default is FALSE.

- persistentCaching:

  If TRUE, caches are persisted to and reloaded from `.improver.cache`
  file. Default is FALSE.

## Value

Invisibly returns `NULL`. Called for its side effects:

- Establishes authenticated connection to the improve repository

- Sets internal configuration

- Initializes logging with specified verbosity level

- Optionally loads persistent cache from `.improver.cache`

After successful connection, use
[`pwd`](https://improverse.github.io/improveR/reference/pwd.md) to
verify the current working step and
[`whoami`](https://improverse.github.io/improveR/reference/whoami.md) to
confirm the authenticated user.

## See also

[`improveConnected`](https://improverse.github.io/improveR/reference/improveConnected.md)
to check if connected,
[`improveDisconnect`](https://improverse.github.io/improveR/reference/improveDisconnect.md)
to close connection,
[`setEditable`](https://improverse.github.io/improveR/reference/setEditable.md)
to enable write operations,
[`whoami`](https://improverse.github.io/improveR/reference/whoami.md) to
get current user,
[`pwd`](https://improverse.github.io/improveR/reference/pwd.md) to get
current step

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic connection using environment variables
# (Set IMPROVER_REPO_URL and IMPROVER_STEP in .Renviron)
improveConnect()

# Verify connection
whoami()
pwd()

# Connection with debug logging for troubleshooting
improveConnect(logLevel = "DEBUG")

# Development connection with self-signed certificates
improveConnect(secure = FALSE)

# Enable write operations after connecting
improveConnect()
setEditable(TRUE)
} # }
```
