validate_ident <- function(ident) {

    # is ident empty?
    is_empty <- function(x) {
        (is.character(x) && length(x) == 1 && x == "") ||
        (is.list(x) && length(x) == 0) ||
        (is.data.frame(x) && nrow(x) == 0)
    }
    
    # is it empty or null? 
    if (is_empty(ident) || is.null(ident) || all(is.na(ident))) {
        return(NULL)
    }
    
    return(ident)
    
}
