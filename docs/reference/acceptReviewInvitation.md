# Accept Review Invitation

Accepts a review invitation as a reviewer. The review must be in
`"Planning"` state and the current user must be an invited reviewer.
After accepting, the reviewer can comment on and approve/reject entries
once the review transitions to `"Reviewing"`.

## Usage

``` r
acceptReviewInvitation(ident, comment = "", from = pwd())
```

## Arguments

- ident:

  Identifier of the review. Can be a path, resource ID, entity ID, or a
  data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- comment:

  Optional plain text comment.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

`TRUE` if the invitation was accepted, `FALSE` otherwise.

## References

ics1476
