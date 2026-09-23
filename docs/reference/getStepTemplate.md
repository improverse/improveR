# Create Step Template

Reads a step and its processes to generate a new step template
environment. This template can be used to create new instances of the
step with a fresh handle.

## Usage

``` r
getStepTemplate(ident)
```

## Arguments

- ident:

  Identifier of the source step to use as a template.

## Value

A step template environment carrying the source step's configuration -
its processes, runserver, tool, tool instance and arguments - ready to
be changed and realised into a new step. Stops with an error, naming the
ident and the reason, when `ident` does not resolve, is not a step, or
has no selected process to take a configuration from (IMR-288).

## References

ics1213

## See also

[`getStep`](https://improverse.github.io/improveR/reference/getStep.md),
[`createStepTemplateEnv`](https://improverse.github.io/improveR/reference/createStepTemplateEnv.md)
