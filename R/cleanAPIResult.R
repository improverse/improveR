#QUESTION function name ok?
convertAPIListToDataframe <- function(httr_cont) {

  #httr_cont <- cont
  #Convert each element: if length is 1, extract it; otherwise keep as list; 
  #shortcomings of alternatives
  #bind_rows: drops empty lists; enframe/pivot_wider: elements of length 1 remain lists;
  processed_list <- lapply(httr_cont, function(x) if (length(x) == 1) x[[1]] else list(x))
    
  #convert to a df with names
  df <- tibble::as_tibble(processed_list)
  return(df)
 
}