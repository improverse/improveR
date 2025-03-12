# REMOVE
# this is a tryout function only; will be removed
# this is a function which adds an element to cacheEnv; the related test will check whether
# the element was indeed added to cacheEnv; purpose is to clarify the cacheEnv issue with
# improveClose()

# addElementToCache <- function(element, value) {
#     assign(element, value, envir = cacheEnv)
# }

addElementToCache <- function(element, value) {
    assign("new_element", "new_value", envir = cacheEnv)
}

removeFile <- function() {
file.remove(file.remove(path_file1))
}
