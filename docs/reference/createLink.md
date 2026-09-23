# Create Link to Resource

Creates a symbolic link to an existing resource within a container
(folder or workflow). Links provide lightweight references to resources
without duplicating content, enabling resource reuse across workflows
while maintaining a single source of truth. Changes to the linked
resource are automatically reflected wherever the link is used.

## Usage

``` r
createLink(linkContainer, links, linkName = "")
```

## Arguments

- linkContainer:

  Identifier of the folder or workflow where the link will be created.
  Can be a path, resource id, or entity id.

- links:

  Identifier(s) of the resource(s) to link to. Can be a single resource
  or multiple resources. Can be a path, resource id, or entity id.

- linkName:

  Character. Optional name for the created link. If not provided (empty
  string), the link uses the name of the target resource. Only
  applicable when creating a single link; ignored when creating multiple
  links.

## Value

The created link resource as a data frame (single link), or a list of
link resources (multiple links). Returns `NULL` if the operation fails
(e.g., target doesn't exist, link already exists, invalid node type for
link creation).

## Details

Links differ from copies in that they reference the target resource
rather than duplicating it. The link always points to the latest version
of the target resource. This is useful for:

- Sharing datasets across multiple workflow steps

- Referencing common templates or configuration files

- Organizing resources without duplication

The function performs validation to ensure:

- Both target container and link source exist

- Target container accepts links (not all node types support links)

- No naming conflicts exist in the target container

- Repository is in editable mode (automatically checked via
  [`improveEditable()`](https://improverse.github.io/improveR/reference/improveEditable.md))

After creating links, related caches are automatically cleared to ensure
fresh data on subsequent queries.

## References

ics1138

## See also

[`copy`](https://improverse.github.io/improveR/reference/copy.md) for
duplicating resources instead of linking,
[`createExternalLink`](https://improverse.github.io/improveR/reference/createExternalLink.md)
for creating external URL references,
[`loadReferences`](https://improverse.github.io/improveR/reference/loadReferences.md)
to view all references from a resource

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a link to a dataset in a workflow step
createLink(
  linkContainer = "/Workflows/Analysis/Step 1",
  links = "/Data/master_dataset.csv"
)

# Create link with custom name
createLink(
  linkContainer = "/Workflows/Analysis/Step 2",
  links = "/Data/master_dataset.csv",
  linkName = "input_data"
)

# Create multiple links at once
createLink(
  linkContainer = "/Workflows/Modeling",
  links = c("/Data/dataset1.csv", "/Data/dataset2.csv")
)
} # }
```
