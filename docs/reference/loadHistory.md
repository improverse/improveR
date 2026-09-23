# Loads the History by the ResourceId, Entity ID or Entity Version ID it uses caching the results are returned as a data frame or a list of data frames the dates are also converted to posix dates via convertImproveTimestampToPosix resourceId can be a list

arguments:

## Usage

``` r
loadHistory(ident, from = pwd())
```

## Arguments

- ident:

  the resource id or the entity id of the resource

- from:

  used if a relative path is used

## Value

A dataframe with the following columns:

- type (character)

- resourceId (character)

- entityId (character)

- entityVersionId (character)

- path (character)

- name (character)

- data (list)

The 'data' column contains a nested dataframe with:

- data/resourceId (character)

- data/resourceVersionId (character)

- data/nodeType (character)

- data/name (character)

- data/deleted (logical)

- data/entityId (character)

- data/entityVersionId (character)

- data/revisionId (character)

- data/lastModifiedOn (numeric)

- data/lastModifiedByName (character)

- data/fileSize (integer)

- data/hasChildren (logical)

- data/hasChildrenIncludingFiles (logical)

- data/path (character)

- data/outdatedLink (logical)

- data/workingFile (logical)

- data/lastModifiedOnDate (POSIXct)

## References

ics1094
