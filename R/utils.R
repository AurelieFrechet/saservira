data_equal_to <- function(code_sas){
  str_match(string = code_sas,
            pattern = "data\\s?=\\s?([0-9a-zA-Z.]+)")[,2]
}


transform_functions <- function(string){
  string %>%  str_replace(pattern = "avg", replacement = "mean")
}
