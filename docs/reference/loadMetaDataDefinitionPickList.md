# Loads One Meta Data Definition Picklist Values By Name For A Scope, The Default Scope Is "Improve Client"

Loads One Meta Data Definition Picklist Values By Name For A Scope, The
Default Scope Is "Improve Client"

## Usage

``` r
loadMetaDataDefinitionPickList(name, scope = "Improve Client")
```

## Arguments

- name:

  name of the meta data definition

- scope:

  the metadata scope

## Value

The pick list values of the definition - the `categoryValues` entry of
the matching row. Only meaningful for a definition of type `LOV`; for a
name that does not exist in the scope the call fails, because there is
no row to take the values from.

## References

ics1137
