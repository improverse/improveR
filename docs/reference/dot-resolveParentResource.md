# Resolve parent resource from relative path using id mapping

Resolve parent resource from relative path using id mapping

## Usage

``` r
.resolveParentResource(parentRelativePath, rootResource, idMapping)
```

## Arguments

- parentRelativePath:

  Relative path of the parent.

- rootResource:

  Root target resource.

- idMapping:

  Environment mapping old resourceIds to new ones.

## Value

Resource data frame for the parent, or rootResource if
parentRelativePath is empty.
