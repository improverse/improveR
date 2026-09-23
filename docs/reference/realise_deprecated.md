# Deprecated realise function for compatibility with older repository versions

This function implements the old realiseStep logic from improveRmodify
for compatibility with repository versions \< 4.4

## Usage

``` r
realise_deprecated(env, force = TRUE, run = TRUE, workflow = NULL)
```

## Arguments

- env:

  The step template environment

- force:

  Force creation of step even if equivalent exists

- run:

  Automatically run the step after creation

## Value

The step template environment (invisibly)
