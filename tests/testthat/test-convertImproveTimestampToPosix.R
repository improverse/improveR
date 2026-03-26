test_that("Does convertImproveTimestampToPosix convert a timestamp to a POSIXct object?", {

  timestamp_1 <- "1736324938243"
  timestamp_2 <- "1736339372152"
  timestamp_3 <- "0"
  timestamps <- c(timestamp_1, timestamp_2, timestamp_3)

  time_res <- c("2025-01-08 09:28:58", "2025-01-08 13:29:32", "1970-01-01 01:00:00")

  expect_equal(convertImproveTimestampToPosix(timestamps) %>% format("%Y-%m-%d %H:%M:%S"), time_res)

})
