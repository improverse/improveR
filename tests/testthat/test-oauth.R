
testthat::test_that("tests the creation of a correct code challenge",{
    code_verifier <- "WUsHGZRCV9NGaRfp9RlaMl4NQvLx8TNtrUj5crJYXH7wTJcaxt4ykP7AAJ41kVtICGfmzdUacdACgQ6y5OlTz6bt9CX1Hc5oyb6F6K5ovlPAQ-GuRBdZlOGw4vsoXSas"
    #example from working reference implementation
    compareCodeChallenge <-"0p4FWNgRCuN6RgIo-NBwklGQPIBvdkd6oXe5PAeZMog"
    code_challenge <- createCodeChallenge(code_verifier)
    testthat::expect_equal(code_challenge, compareCodeChallenge)

    

# Encode the raw hash using standard base64 encoding with line breaks
# openssl::base64_encode(hash_raw, linebreaks = TRUE)

# Decode the compareCodeChallenge using base64url decoding
# jose::base64url_decode(compareCodeChallenge)
})


