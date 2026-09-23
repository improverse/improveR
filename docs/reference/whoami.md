# Get Current Authenticated User

Retrieves the username of the currently authenticated user or the user
who initiated the current workflow step.

## Usage

``` r
whoami()
```

## Value

A character string containing the username. Returns either:

- The authenticated user from the current session configuration, or

- The username from the audit trail of the workflow run that initiated
  the current step (fallback when session user is not set)

## Details

The function uses a two-tier approach to identify the user:

1.  **Primary**: Checks the session configuration for the authenticated
    user (set during
    [`improveConnect()`](https://improverse.github.io/improveR/reference/improveConnect.md))

2.  **Fallback**: If no session user is found, queries the audit trail
    to identify who created the current workflow run

This dual mechanism ensures user identification even in batch processing
scenarios where explicit authentication may not be present.

## References

ics1251

## See also

[`improveConnect`](https://improverse.github.io/improveR/reference/improveConnect.md)
for authentication,
[`loadAuditTrail`](https://improverse.github.io/improveR/reference/loadAuditTrail.md)
for audit trail access,
[`pwd`](https://improverse.github.io/improveR/reference/pwd.md) for
current step information

## Examples

``` r
if (FALSE) { # \dontrun{
# Connect to improve server
improveConnect()

# Get current user
current_user <- whoami()
cat("Current user:", current_user, "\n")

# Use in logging or audit contexts
log_message <- paste("Analysis performed by:", whoami())

# Check permissions before operations
if (whoami() %in% authorized_users) {
  # Perform sensitive operation
}
} # }
```
