# Pure helper test: no server, no connection, milliseconds.
# Recovered from _test-oauth.R, where it sat next to six httptest mock blocks.
# That file is gone (IMR-303): httptest was rejected by ENT-05, its recordings
# never existed, and it was the only thing keeping httptest in Suggests.
# whose recordings do not exist, so the whole file never ran (IMR-263/IMR-264).
# createCodeChallenge builds the PKCE code challenge of the OAuth device flow
# described in ics2071 and was covered by no test at all.
# Qualified with improveR::: so the check runs against the installed package.

test_that("createCodeChallenge derives the PKCE challenge from the verifier|ics2071", {
  codeVerifier <- paste0("WUsHGZRCV9NGaRfp9RlaMl4NQvLx8TNtrUj5crJYXH7wTJcaxt4ykP7A",
                         "AJ41kVtICGfmzdUacdACgQ6y5OlTz6bt9CX1Hc5oyb6F6K5ovlPAQ-Gu",
                         "RBdZlOGw4vsoXSas")
  expect_equal(improveR:::createCodeChallenge(codeVerifier),
               "0p4FWNgRCuN6RgIo-NBwklGQPIBvdkd6oXe5PAeZMog")
})
