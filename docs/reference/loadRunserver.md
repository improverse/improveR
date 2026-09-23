# Loads Runserver By Label

`loadRunserver()` loads a runserver by its label and returns a dataframe
with pertaining details.

## Usage

``` r
loadRunserver(label)
```

## Arguments

- label:

  the label of the runserver

## Value

A dataframe with the following columns:

- `id` (character)

- `url` (character)

- `hostname` (character)

- `label` (character)

- `local` (logical)

- `deleted` (logical)

- `generic` (logical)

- `reproducible` (logical)

- `sshPort` (integer)

## References

ics1226
