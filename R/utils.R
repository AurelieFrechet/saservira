data_equal_to <- function(code_sas){
  str_match(chaine = code_sas,
            pattern = "data\\s?=\\s?([0-9a-zA-Z.]+)")[,2]
}

transform_casewhen <- function(chaine){
  chaine <- chaine %>%
    str_remove_all(pattern = "case") %>%
    str_remove_all(pattern = "end") %>%
    str_remove_all(pattern = "\n")

  when_then <-
    str_match_all(string = chaine,
                  pattern = "when\\s+([\\S]+)\\s+then\\s+([\\S]+)")[[1]]

  requete <-
    paste(when_then[, 2], when_then[, 3], sep = " ~ ") %>%
    paste(., collapse = ",\n")

  else_then <-
    str_match_all(string = chaine,
                  pattern = "else\\s+([\\S]+)")[[1]]

  if (length(else_then) == 1) {
    requete <- paste0(requete,
                     ",\nTRUE ~ "?
                       else_then[,2])
  }

  requete <- paste0("case_when(", requete,")")

  return(requete)

}

#' transform functions
#'
#' @description remplace les fonctions par défaut SAS ou SQL par des fonctions R
#' @param chaine chaine de charactères contenant la fonction
transform_functions <- function(chaine){
  chaine_casewhen <- str_extract_all(chaine = chaine,
                                     pattern = "case ([\\s\\S]+) end")
  chaine %>%

    # Fonctions de base
    str_replace_all(pattern = "false",          replacement = "FALSE")%>%
    str_replace_all(pattern = "true",           replacement = "TRUE") %>%
    str_replace_all(pattern = "avg",            replacement = "mean") %>%
    str_replace_all(pattern = "var_samp",       replacement = "var")  %>%
    str_replace_all(pattern = "stddev_samp",    replacement = "sd")   %>%
    str_replace_all(pattern = "count\\(\\*\\)", replacement = "n()")  %>%
    str_replace_all(pattern = "count\\(distinct\\(([a-zA-z0-9_]+)\\)\\)",
                    replacement = "n_distinct(\\1)") %>%

    # Case when

    str_replace_all(pattern = "case ([\\s\\S]+) end",
                    replacement = "case_when(\\1)") %>%
    str_replace_all(pattern = "when ([\\s\\S]+) then ([\\S]+)\\)",
                    replacement = "\\1 ~ \\2)") %>%
    str_replace_all(pattern = "when ([\\s\\S]+) then ([\\S]+)",
                    replacement = "\\1 ~ \\2,")

}

#' transform pattern
#' @description remplace les motifs d'intentification de caractères des LIKES
#' dans les clauses WHERE d'une requête SQL
#' @param chaine chaine de caractères de la clause
transform_pattern  <- function(chaine){

}


#' transform condition
#' @description remplace les opérateurs de construction des prédicats pour
#' qu'ils soient compatibles avec R
#' @param chaine chaine de caractère contenant les conditions
transform_conditions <- function(chaine){
  chaine %>%
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
