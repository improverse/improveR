# List All Users in improve Repository

Retrieves a list of all users registered in the improve repository.

## Usage

``` r
users()
```

## Value

A data frame containing user information with columns:

- id:

  Character. Unique user identifier

- username:

  Character. Username for authentication

- firstName:

  Character. User's first name

- lastName:

  Character. User's last name

- email:

  Character. User's email address

- active:

  Logical. Whether the user account is active

- createdAt:

  POSIXct. Creation timestamp

- lastModified:

  POSIXct. Last modification timestamp

Returns `NULL` if the user list cannot be retrieved.

## References

ics1142

## See also

[`whoami`](https://improverse.github.io/improveR/reference/whoami.md) to
get the current user

## Examples

``` r
if (FALSE) { # \dontrun{
# List all users
all_users <- users()

# Find a specific user
admin_user <- all_users[all_users$username == "admin", ]
} # }
```
