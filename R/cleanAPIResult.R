# @HACKLM # QUESTION
# As for the capturing of the new/changed API response:
# - AFAICT, the previous error occured when 
#
#   cont <- httr::content(result)
#   df <- as.data.frame(cont, stringAsFactors = FALSE) was called (row 42 in loadResourceFromServer.R)
#   Error in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  :       
#     arguments imply differing number of rows: 1, 0
#
# This error originated from the presence of empty lists in cont. The question is whether
# these empty lists are of any present or future relevance. The suggested fix entry<-Filter(function(e){!(is.list(e)&&length(e)==0)},entry)
# drops empty lists from the result (an alternative with the same result would be simply dplyr::bind_rows(cont))
# If we want - for whatever reason - to keep empty lists, the propsed function below would keep them as columns of class list in the dataframe.

convertAPIListToDataframe <- function(httr_cont) {

  processed_list <- lapply(httr_cont, function(x) if (length(x) == 1) x[[1]] else list(x))

  #convert to a tibble() with names
  df <- tibble::as_tibble(processed_list) %>% as.data.frame()
  return(df)
  
}
