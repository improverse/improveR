# Create External Link in improve Repository

Creates a link resource pointing to an external URL (outside the
repository), such as a database, website, or external documentation
system.

## Usage

``` r
createExternalLink(
  targetIdent,
  linkName = "",
  url,
  comment = "Created by improveR"
)
```

## Arguments

- targetIdent:

  Identifier of the parent folder, analysis tree, or step where the link
  should be created.

- linkName:

  Character. Name of the link resource.

- url:

  Character. The destination URL (e.g., "https://example.com" or
  "db://server").

- comment:

  Character. Comment for the creation audit entry. Defaults to "Created
  by improveR".

## Value

A list containing the resource object of the created external link.

## References

ics1104

## See also

[`createFile`](https://improverse.github.io/improveR/reference/createFile.md)
