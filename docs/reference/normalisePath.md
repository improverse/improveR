# normalisePath

normalisePath converts any path to an absolute path, and eliminates all
.. and . If a relative path is given, it needs an absolute path as
startPath to resolve. If by too many .. the path navigates beyond the
root, NULL is returned.

## Usage

``` r
normalisePath(path, startPath = "/")
```

## Arguments

- path:

  the path to normalise

- startPath:

  Starting point for relative paths, defaults to /

## Value

The absolute path as a character string, with every `.` and `..`
resolved and backslashes turned into forward slashes. `NULL` when the
path navigates beyond the root - that is the documented way of saying
"no such path", not an error.

## References

ics1089

## Examples

``` r
if (FALSE) { # \dontrun{
normalisePath(path = "./../lmer/../lmer", startPath = "/0demo/lmer") # /0demo/lmer
normalisePath(path = "./../../../lmer/../lmer", startPath = "/0demo/lmer") # NULL
normalisePath(path = "/0demo/lmer", startPath = "/0demo/lmer") # /0demo/lmer
} # }
```
