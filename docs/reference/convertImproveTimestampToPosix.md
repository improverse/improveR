# Convert Improve Timestamp to POSIX

Converts timestamps from the improve platform format (milliseconds since
Unix epoch) to R's POSIX datetime objects. Handles single values,
vectors, and lists.

## Usage

``` r
convertImproveTimestampToPosix(timestamp)
```

## Arguments

- timestamp:

  Timestamp(s) to convert. Can be:

  - Single character or numeric value (milliseconds since 1970-01-01)

  - Character or numeric vector

  - List of timestamps

## Value

A POSIXct datetime object (single value) or a POSIXct vector (multiple
values). Times are converted from milliseconds to seconds and offset
from the Unix epoch (1970-01-01 00:00:00 UTC). The timezone defaults to
the system's current timezone.

## Details

The improve platform stores timestamps as milliseconds since the Unix
epoch (1970-01-01 00:00:00 UTC), following JavaScript/Java conventions.
This function converts them to R's POSIX datetime objects for use with
standard R date/time functions.

The conversion process:

1.  Divides milliseconds by 1000 to convert to seconds

2.  Creates POSIX time using 1970-01-01 as origin

3.  Applies the system's current timezone (not UTC)

**Note:** The timezone defaults to the current system timezone, not UTC.
This means the same timestamp may display differently on systems in
different timezones. If consistent UTC times are needed across systems,
consider converting the result explicitly to UTC using
`attr(result, "tzone") <- "UTC"`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert single timestamp
timestamp <- "1609459200000"  # 2021-01-01 00:00:00 UTC
convertImproveTimestampToPosix(timestamp)

# Convert vector of timestamps
timestamps <- c("1609459200000", "1609545600000")
convertImproveTimestampToPosix(timestamps)

# Use with improve resource timestamps
resource <- loadResource("/Data/file.csv")
created_time <- convertImproveTimestampToPosix(resource$created)
modified_time <- convertImproveTimestampToPosix(resource$modified)
} # }
```
