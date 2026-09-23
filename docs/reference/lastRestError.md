# Last REST Error

Returns details about the last failed REST call, or NULL if the last
call succeeded. Since improveR is single-threaded, this is safe to use
after any REST operation.

## Usage

``` r
lastRestError()
```

## Value

A list with `status_code`, `url`, `method`, `message`, and `timestamp`,
or `NULL` if the last call was successful.
