# Unloads All Relation Types

Unloads All Relation Types

## Usage

``` r
unloadRelationTypes()
```

## Value

No meaningful value - called for its side effect of dropping the
relation types from the cache, so that the next load reads the server.
The value handed back by the internal cache removal is an implementation
detail and must not be relied on.
