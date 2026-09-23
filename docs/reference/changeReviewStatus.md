# Change Review Status

Changes the status of a review. Reviews have three statuses with the
following transitions:

## Usage

``` r
changeReviewStatus(ident, newStatus, from = pwd())
```

## Arguments

- ident:

  Identifier of the review. Can be a path, resource ID, entity ID, or a
  data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- newStatus:

  Character. The target status: `"Reviewing"`, `"Approved"`, or
  `"Planning"`.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

`TRUE` if the status was changed successfully, `FALSE` otherwise.

## Details

- **Planning**:

  Initial state. Entries and reviewers are being set up.

- **Reviewing**:

  Active review. Reviewers can approve/reject entries. Requires at least
  one entry and one reviewer.

- **Approved**:

  Final/locked state. Both the review and all reviewed resources are
  locked. Only an admin can revert to Reviewing.

Valid transitions:

- `Planning -> Reviewing` (requestor or admin, needs entries + reviewer)

- `Reviewing -> Approved` (requestor or admin, all entries must be
  reviewed, no rejections)

- `Reviewing -> Planning` (requestor or admin, resets all approvals)

- `Approved -> Reviewing` (admin only, reopens a locked review)

## References

ccs27

## Examples

``` r
if (FALSE) { # \dontrun{
# Start the review process
changeReviewStatus(review, "Reviewing")

# Approve and lock the review (all entries must be reviewed first)
changeReviewStatus(review, "Approved")

# Revert to planning (resets approvals)
changeReviewStatus(review, "Planning")
} # }
```
