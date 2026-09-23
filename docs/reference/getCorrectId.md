# getCorrectId

Resolves various ident types to an ID.

- a resource =\> resource ID

- a resource ID =\> resource ID

- a resource versionID =\> resource version ID

- a long entity ID with http: ... =\> short entity ID

- a short entity ID =\> short entity ID

- an entity ID without prefix =\> short entity ID

## Usage

``` r
getCorrectId(resolveToId)
```

## Arguments

- resolveToId:

  Value which should be resolved to an ID. Accepts a resource ID,
  resource version ID, long and short entity ID, and an entity without
  prefix.

## Value

The resolved identifier as a character string: the `resourceId` column
for a data frame, the part after `=` for a long entity id in URL form
(shortened to the repository prefix when the host is spelled out), the
value prefixed with
[`repoPrefix()`](https://improverse.github.io/improveR/reference/repoPrefix.md)
for an entity id without prefix, and the input unchanged for a path or a
value that already carries a prefix. Stops with an error for `NULL`, an
empty value, a data frame without `resourceId`, or a non-character
value.

## References

ics1087

## Examples

``` r
if (FALSE) { # \dontrun{
getCorrectId("112EE78F4CDC4400836F8C059AF2EA5F") #resourceId
getCorrectId("02C347E7439942FE834C7714F49EF082") #resourceVersionId
getCorrectId("http://host/provide?resourceId=resourceId=my_server:ST-63657") #long entity id
getCorrectId("my_server:ST-63657") #short entity id
} # }
```
