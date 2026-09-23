# Log_warn

Log_warn

## Usage

``` r
log_warn(...)
```

## Arguments

- ...:

  combines all items to one log message

## Value

No meaningful value - called for its side effect of writing the message.
Where the log is redirected to a structured list (internal
`redirectLogs`) the message is appended to that list under the current
context; otherwise it is handed to the `logging` package.
