#' Convert API list response to dataframe
#' Handles empty lists in API responses by preserving them as list columns
#' @param httr_cont Content from httr response
#' @return dataframe with preserved list columns
convertAPIListToDataframe <- function(httr_cont) {

  processed_list <- lapply(httr_cont, function(x) if (length(x) == 1) x[[1]] else list(x))

  #convert to a tibble() with names
  df <- tibble::as_tibble(processed_list) %>% as.data.frame()
  return(df)
  
}
