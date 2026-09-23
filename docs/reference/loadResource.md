# Load Resource

Loads one or multiple resources.

## Usage

``` r
loadResource(ident, from = pwd())
```

## Arguments

- ident:

  id

- from:

  Path working directory.

## Value

A data frame with the fields `resourceId`, `entityId`,
`entityVersionId`, `path`, `name`, and `data`.

## Details

### ident

There are multiple ways to describe the ident of a resource:

Absolute idents:

- resourceId: a UUID

- Data frame: uses the resourceId value of the data frame

- entityId: pointer to the latest version of a resource. Short and long
  entityIds are accepted'

- entityVersionId: pointer to a specific version of a resource. Short
  and long entityIds are accepted'

- path: the full path to a resource, always starting with /.

Relative idents:

- All relative idents are path based, they always have to start with ./
  or ../'

- Relative path without pwd: always starts from the return value of
  pwd()'

- Relative path and pwd as second argument: starts the relative path
  from the absolute ident that was handed over as second argument.
  Sometimes still called “from” but will be updated to pwd.

### pwd

pwd shows the current “path working directory”. The improveR client is
inspired by a command line interface. The default start position within
the improve repository is the step that started an R instance with
improveR.This starting point is set via the IMPROVER_STEP environment
variable. If this information was not provided the root element is pwd.
pwd is used if you use relative paths to access an element.

## References

ics1090

## Examples

``` r
if (FALSE) { # \dontrun{
loadResource("112EE78F4CDC4400836F8C059AF2EA5F") #resourceId
loadResource("your_server:ST-63657") #entityId
loadResource("your_server:ST-63657-1") #entityVersionId
loadResource("/projects/folder/analysis_tree/Step 3") #path
} # }
```
