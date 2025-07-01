xorRawToRaw <-function(valueRaw,keyRaw) {
  valueLength <- length(valueRaw)
  keyLength <- length(keyRaw)
  if (valueLength==0) {
    return (NULL)
  }
  xored <- c()
  for (i in 1:valueLength) {
    xored <- c(xored,xor(valueRaw[i],keyRaw[(i%%keyLength)+1]))
  }
  return(xored)
}

#' encrypts a string with a one time pad
#'
#' encryption using xor
#'
#' @param value the value to encrypt
#' @param key the key
#' @export
xorEncrypt <- function(value,key) {
  value <- openssl::base64_encode(value)
  valueRaw<-charToRaw(value)
  keyRaw <-charToRaw(key)

  xored <- xorRawToRaw(valueRaw,keyRaw)
  if (is.null(xored)) {
    return(NULL)
  }
  return (openssl::base64_encode(xored))
}

#' decrypts a string with a one time pad
#'
#' decryption using xor
#'
#' @param value the value to decrypt
#' @param key the key
#' @export
xorDecrypt <- function(value,key) {
  valueRaw<-openssl::base64_decode(value)
  keyRaw <-charToRaw(key)
  xored <- xorRawToRaw(valueRaw,keyRaw)
  if (is.null(xored)) {
    return(NULL)
  }
  return (rawToChar(openssl::base64_decode(rawToChar(xored))))
}
