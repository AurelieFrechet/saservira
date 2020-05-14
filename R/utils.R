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
    str_replace_all(pattern = "avg",            replacement = "mean") %>%
    str_replace_all(pattern = "var_samp",       replacement = "var")  %>%
    str_replace_all(pattern = "stddev_samp",    replacement = "sd")   %>%
    str_replace_all(pattern = "count\\(\\*\\)", replacement = "n()")  %>%
    str_replace_all(pattern = "count\\(distinct\\(([a-zA-z0-9_]+)\\)\\)",
                    replacement = "n_distinct(\\1)")

}

#' transform pattern
#' @description remplace les motifs d'intentification de caractères des LIKES
#' dans les clauses WHERE d'une requête SQL
#' @param string chaine de caractères de la clause
transform_pattern  <- function(string){

}


#' transform condition
#' @description remplace les opérateurs de construction des prédicats pour
#' qu'ils soient compatibles avec R
#' @param string chaine de caractère contenant les conditions
transform_conditions <- function(string){
  string %>%
    # Gestion NULL et .
    str_replace(pattern = "([a-zA-Z0-9.]+)\\s?=\\s?\\.",
                replacement = "is.na(\\1)") %>%
    str_replace(pattern = "([a-zA-Z0-9.]+)\\s?<>\\s?\\.",
                replacement = "!is.na(\\1)") %>%
    str_replace(pattern = "([a-zA-Z0-9.]+)\\sis\\snull",
                replacement = "is.na(\\1)") %>%
    str_replace(pattern = "([a-zA-Z0-9.]+)\\sis\\s\\not\\snull",
                replacement = "!is.na(\\1)") %>%

    # Remplacement =/le/ge/<>
    str_replace_all(pattern = "\\s?=\\s?",  replacement = " == ") %>%
    str_replace_all(pattern = "\\sge\\s",   replacement = " >= ") %>%
    str_replace_all(pattern = "\\sle\\s",   replacement = " <= ") %>%
    str_replace_all(pattern = "\\s?<>\\s?", replacement = " != ") %>%

    # Remplacement NOT IN
    str_replace(pattern = "([a-zA-Z0-9.]+)\\snot\\sin\\s([a-zA-Z0-9,()]+)",
                replacement = "!(\\1 %in% c\\2)") %>%

    # Remplacement IN
    str_replace(pattern = "([a-zA-Z0-9.]+)\\sin\\s([a-zA-Z0-9,()]+)",
                replacement = "\\1 %in% c\\2") %>%

    # Remplacement BETWEEN
    str_replace(pattern = "([a-zA-Z0-9.]+)\\sbetween\\s(\\w+)\\sand\\s(\\w+)",
                replacement = "between(\\1, \\2, \\3)") %>%

    # Remplacement LIKES
    # TODO

    # Remplacement and et or
    str_replace_all(pattern = "\\s?and\\s?",  replacement = " & ") %>%
    str_replace_all(pattern = "\\s?or\\s?",   replacement = " | ") %>%
    str_replace_all(pattern = "\\s?not\\s?",  replacement = " !")
}
