# Checks if httptest (and only httptest) ic currently capturing the results by checking the trace on httr::POST


isCapturing <- function() {
  any(grepl("functionWithTrace",utils::capture.output(httr::POST)))
}
