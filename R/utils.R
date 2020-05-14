data_equal_to <- function(code_sas){
  str_match(string = code_sas,
            pattern = "data\\s?=\\s?([0-9a-zA-Z.]+)")[,2]
}


#' transform functions
#'
#' @description remplace les fonctions par défaut SAS ou SQL par des fonctions R
#' @param string chaine de charactères contenant la fonction
transform_functions <- function(string){
  string %>%
    str_replace(pattern = "avg", replacement = "mean")
}

#' transform pattern
#' @description remplace les motifs d'intentification de caractères des LIKES
#' dans les clauses WHERE d'une requête SQL
#' @param string chaine de caractères de la clause
transform_pattern  <- function(string){

}
