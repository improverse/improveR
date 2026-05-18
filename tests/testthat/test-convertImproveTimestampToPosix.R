test_that("Does convertImproveTimestampToPosix convert a timestamp to a POSIXct object?|ics1083", {

  timestamp_1 <- "1736324938243"
  timestamp_2 <- "1736339372152"
  timestamp_3 <- "0"
  timestamps <- c(timestamp_1, timestamp_2, timestamp_3)

  time_res <- c("2025-01-08 09:28:58", "2025-01-08 13:29:32", "1970-01-01 01:00:00")

  # Compare in a fixed TZ so the test does not depend on the container/host TZ.
  # The expected strings above are in Europe/Vienna (CET/CEST, UTC+1/+2).
  expect_equal(convertImproveTimestampToPosix(timestamps) %>% format("%Y-%m-%d %H:%M:%S", tz = "Europe/Vienna"), time_res)

})
