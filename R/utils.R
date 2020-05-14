data_equal_to <- function(code_sas){
  str_match(string = code_sas,
            pattern = "data\\s?=\\s?([0-9a-zA-Z.]+)")[,2]
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
    str_replace(pattern = regex("([a-zA-Z0-9.]+)\\sis\\snull", ignore_case = T),
                replacement = "is.na(\\1)") %>%
    str_replace(pattern = regex("([a-zA-Z0-9.]+)\\sis\\s\\not\\snull", ignore_case = T),
                replacement = "!is.na(\\1)") %>%

    # Remplacement =/le/ge/<>
    str_replace_all(pattern = "\\s?=\\s?",  replacement = " == ") %>%
    str_replace_all(pattern = regex("\\sge\\s", ignore_case = T),   replacement = " >= ") %>%
    str_replace_all(pattern = regex("\\sle\\s", ignore_case = T),   replacement = " <= ") %>%
    str_replace_all(pattern = "\\s?<>\\s?", replacement = " != ") %>%

    # Remplacement NOT IN
    str_replace(pattern = regex("([a-zA-Z0-9.]+)\\snot\\sin\\s([a-zA-Z0-9,()]+)", ignore_case = T),
                replacement = "!(\\1 %in% c\\2)") %>%

    # Remplacement IN
    str_replace(pattern = regex("([a-zA-Z0-9.]+)\\sin\\s([a-zA-Z0-9,()]+)", ignore_case = T),
                replacement = "\\1 %in% c\\2") %>%

    # Remplacement BETWEEN
    str_replace(pattern = regex("([a-zA-Z0-9.]+)\\sbetween\\s(\\w+)\\sand\\s(\\w+)", ignore_case = T),
                replacement = "between(\\1, \\2, \\3)") %>%

    # Remplacement LIKES
    # TODO

    # Remplacement and et or
    str_replace_all(pattern = regex("\\s?and\\s?", ignore_case = T),  replacement = " & ") %>%
    str_replace_all(pattern = regex("\\s?or\\s?", ignore_case = T),   replacement = " | ") %>%
    str_replace_all(pattern = regex("\\s?not\\s?", ignore_case = T),  replacement = " !")
}

transform_casewhen <- function(chaine){
  chaine <- chaine %>%
    str_remove_all(pattern = regex("case", ignore_case = T)) %>%
    str_remove_all(pattern = regex("end", ignore_case = T)) %>%
    str_remove_all(pattern = "\n")

  when_then <-
    str_match_all(
      string = chaine,
      pattern = regex("when\\s+([\\S]+)\\s+then\\s+([\\S]+)", ignore_case = T)
    )[[1]]

  requete <-
    paste(when_then[, 2], when_then[, 3], sep = " ~ ") %>%
    paste(., collapse = ",\n")

  else_then <-
    str_match_all(string = chaine,
                  pattern = regex("else\\s+([\\S]+)", ignore_case = T))[[1]]

  if (length(else_then) > 0) {
    requete <- paste0(requete,
                      ",\nTRUE ~ ",
                        else_then[, 2])
  }

  requete <- paste0("case_when(", requete, ")")

  return(requete)

}

#' transform functions
#'
#' @description remplace les fonctions par défaut SAS ou SQL par des fonctions R
#' @param chaine chaine de charactères contenant la fonction
transform_functions <- function(chaine){

  chaine <- chaine %>%
    # Fonctions de base
    str_replace_all(pattern = regex("false", ignore_case = T),          replacement = "FALSE")%>%
    str_replace_all(pattern = regex("true", ignore_case = T),           replacement = "TRUE") %>%
    str_replace_all(pattern = regex("avg", ignore_case = T),            replacement = "mean") %>%
    str_replace_all(pattern = regex("var_samp", ignore_case = T),       replacement = "var")  %>%
    str_replace_all(pattern = regex("stddev_samp", ignore_case = T),    replacement = "sd")   %>%
    str_replace_all(pattern = regex("count\\(\\*\\)", ignore_case = T), replacement = "n()")  %>%
    str_replace_all(pattern = regex("count\\(distinct\\(([a-zA-z0-9_]+)\\)\\)", ignore_case = T),
                    replacement = "n_distinct(\\1)")

    # Case when

  chaine_casewhen <- str_extract_all(string = chaine,
                                     pattern = regex("case\\s([\\s\\S]+)\\send", ignore_case = T))[[1]]

  if (!identical(chaine_casewhen, character(0))) {
    chaine <-
      str_replace_all(
        string = chaine,
        pattern = regex("case\\s([\\s\\S]+)\\send", ignore_case = T),
        replacement = transform_casewhen(chaine_casewhen)
      )
  }

  # Changement des predicats
  chaine <- transform_conditions(chaine)

  return(chaine)
}


