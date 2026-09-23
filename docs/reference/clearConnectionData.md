# Clear Connection Data

deletes repoUrl, stepId, and token from the environment variables and
calls improveDisconnect

## Usage

``` r
clearConnectionData(includeRepoData = F)
```

## Arguments

- includeRepoData:

  includes the repository URL and the selected step in the clear process

## Value

No meaningful value - called for its side effects on the environment
variables, the CLI user profile and the connection. The value passed
through from
[`improveDisconnect()`](https://improverse.github.io/improveR/reference/improveDisconnect.md)
is an implementation detail and must not be relied on.

## See also

[`improveDisconnect()`](https://improverse.github.io/improveR/reference/improveDisconnect.md)
