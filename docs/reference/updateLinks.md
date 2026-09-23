# Update Link Resources to Latest Target Version

Refreshes one or more link resources to point to the latest version of
their targets. Links in improve always reference the most recent version
of a resource, but this function explicitly triggers an update and cache
refresh, useful when target resources have changed and you want to
ensure links are synchronized.

## Usage

``` r
updateLinks(links, comment = "update outdated")
```

## Arguments

- links:

  Identifier(s) of link resource(s) to update. Can be:

  - Vector of resource IDs or entity IDs

  - Data frame with an `entityId` column (extracts IDs automatically)

- comment:

  Character. Optional comment describing the update reason. Defaults to
  "update outdated".

## Value

Invisible `NULL`. Called for its side effect of updating link resources
and clearing related caches.

## Details

The function processes each link by:

1.  Loading the resource and verifying it's a Link node type

2.  Sending an update request to the server with `updateLink=true`

3.  Clearing caches for the link and its parent folder

Only resources of type "Link" are updated - other node types are
silently skipped. This allows you to safely pass a mixed set of
resources and only links will be refreshed.

The function handles duplicate IDs automatically, processing each unique
link only once.

After updating, the parent folder's child cache is cleared to ensure
subsequent queries return fresh data reflecting the updated links.

## See also

[`createLink`](https://improverse.github.io/improveR/reference/createLink.md)
to create new links,
[`loadReferences`](https://improverse.github.io/improveR/reference/loadReferences.md)
to view link relationships,
[`delete`](https://improverse.github.io/improveR/reference/delete.md) to
remove outdated links

## Examples

``` r
if (FALSE) { # \dontrun{
# Update a single link
updateLinks("/Workflows/Step1/data_link")

# Update multiple links
updateLinks(c(
  "/Workflows/Step1/data_link",
  "/Workflows/Step2/data_link"
))

# Update all links in a workflow found by query
links <- query("nodeType='Link' AND path='/Workflows/Analysis/*'")
updateLinks(links, comment = "refresh after data update")

# Update using entity IDs from a data frame
link_df <- data.frame(
  entityId = c("abc123", "def456"),
  name = c("link1", "link2")
)
updateLinks(link_df)
} # }
```
