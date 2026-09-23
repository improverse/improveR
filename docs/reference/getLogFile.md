# Get Log File

Returns the path to the current log file, or NULL if logging to file is
not enabled.

## Usage

``` r
getLogFile()
```

## Value

Character string with the log file path, or NULL if no log file is
configured

## Examples

``` r
# Get current log file path
log_file <- getLogFile()
if (!is.null(log_file)) {
  cat("Logs are being written to:", log_file, "\n")
}
```
