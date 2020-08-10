#' data_equal_to
#' @description detecte la valeur da data et la renvoie, contenur dans une
#' fonction file.path si elle est associée à une librairie SAS
#' @param code_sas
data_equal_to <- function(code_sas){
  data_equal <- str_match(string = code_sas,
            pattern = "data\\s?=\\s?([0-9a-zA-Z._]+)")[,2]

  if (str_detect(data_equal, "\\.")){
    data_equal <- data_equal %>%
      str_split(pattern = "\\.") %>%
      unlist()

    data_equal <- paste0("file.path(", data_equal[1],", \"", data_equal[2], "\")")
  }

  return(data_equal)
}

#' transform pattern
#' @description remplace les motifs d'intentification de caractères des LIKES
#' dans les clauses WHERE d'une requête SQL
#' @param chaine chaine de caractères de la clause
transform_pattern  <- function(chaine){
#TODO
}


#' transform condition
#' @description remplace les opérateurs de construction des prédicats pour
#' qu'ils soient compatibles avec R
#' @param chaine chaine de caractère contenant les conditions
transform_conditions <- function(chaine){
  chaine %>%
    # Gestion NULL et .
    str_replace(pattern = "([\\S]+)\\snot\\s?=\\s?\\.",
                replacement = "!is.na(\\1)") %>%
    str_replace(pattern = "([\\S]+)\\s?=\\s?\\.",
                replacement = "is.na(\\1)") %>%
    str_replace(pattern = regex("([\\S]+)\\sne\\s?\\.", ignore_case = T),
                replacement = "!is.na(\\1)") %>%
    str_replace(pattern = "([\\S]+)\\s?<>\\s?\\.",
                replacement = "!is.na(\\1)") %>%
    str_replace(pattern = regex("([\\S]+)\\sis\\snull", ignore_case = T),
                replacement = "is.na(\\1)") %>%
    str_replace(pattern = regex("([\\S]+)\\sis\\snot\\snull", ignore_case = T),
                replacement = "!is.na(\\1)") %>%

    # Remplacement =/le/ge/<>
    str_replace_all(pattern = "\\s?=\\s?",  replacement = " == ") %>%
    str_replace_all(pattern = regex("\\sne\\s", ignore_case = T),   replacement = " != ") %>%
    str_replace_all(pattern = regex("\\sge\\s", ignore_case = T),   replacement = " >= ") %>%
    str_replace_all(pattern = regex("\\sle\\s", ignore_case = T),   replacement = " <= ") %>%
    str_replace_all(pattern = "\\s?<>\\s?", replacement = " != ") %>%

    # Remplacement NOT IN
    str_replace(pattern = regex("([\\S]+)\\snot\\sin\\s([a-zA-Z0-9,()]+)", ignore_case = T),
                replacement = "!(\\1 %in% c\\2)") %>%

    # Remplacement IN
    str_replace(pattern = regex("([\\S]+)\\sin\\s([a-zA-Z0-9,()]+)", ignore_case = T),
                replacement = "\\1 %in% c\\2") %>%

    # Remplacement NOT BETWEEN
    str_replace(pattern = regex("([\\S]+)\\snot\\sbetween\\s(\\w+)\\sand\\s(\\w+)", ignore_case = T),
                replacement = "!between(\\1, \\2, \\3)") %>%

    # Remplacement BETWEEN
    str_replace(pattern = regex("([\\S]+)\\sbetween\\s(\\w+)\\sand\\s(\\w+)", ignore_case = T),
                replacement = "between(\\1, \\2, \\3)") %>%

    # Remplacement LIKES
    # TODO

    # Remplacement and et or
    str_replace_all(pattern = regex("\\s?\\band\\b\\s?", ignore_case = T),  replacement = " & ") %>%
    str_replace_all(pattern = regex("\\s?\\bor\\b\\s?", ignore_case = T),   replacement = " | ") %>%
    str_replace_all(pattern = regex("\\s?\\bnot\\b\\s?", ignore_case = T),  replacement = " !")
}

transform_casewhen <- function(chaine){
  chaine <- chaine %>%
    str_remove_all(pattern = regex("\\bcase\\b", ignore_case = T)) %>%
    str_remove_all(pattern = regex("\\bend\\b", ignore_case = T)) %>%
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

  requete <- paste0("case_when(", requete, ")") %>%
    transform_conditions()

  return(requete)

}

#' transform functions
#'
#' @description remplace les fonctions par défaut SAS ou SQL par des fonctions R
#' @param chaine chaine de charactères contenant la fonction
transform_functions <- function(chaine){

  chaine <- chaine %>%
    # Fonctions de base SQL
    str_replace_all(pattern = regex("\\bfalse\\b", ignore_case = T),          replacement = "FALSE")%>%
    str_replace_all(pattern = regex("\\btrue\\b", ignore_case = T),           replacement = "TRUE") %>%
    str_replace_all(pattern = regex("\\bavg\\b", ignore_case = T),            replacement = "mean") %>%
    str_replace_all(pattern = regex("\\bvar_samp\\b", ignore_case = T),       replacement = "var")  %>%
    str_replace_all(pattern = regex("\\bstddev_samp\\b", ignore_case = T),    replacement = "sd")   %>%
    str_replace_all(pattern = regex("\\bcount\\(\\*\\)", ignore_case = T), replacement = "n()")  %>%
    str_replace_all(pattern = regex("\\bcount\\(distinct\\(([a-zA-z0-9._]+)\\)\\)", ignore_case = T),
                    replacement = "\\bn_distinct(\\1)") %>%

  # Indicateurs proc means

    str_replace_all(
      pattern = regex("\\bKURT\\b",
                      ignore_case = T),
      replacement = "kurtosis"
    ) %>%
    str_replace_all(
      pattern = regex("\\bLCLM\\b",
                      ignore_case = T),
      replacement = "t.test"
    ) %>%
    str_replace_all(
      pattern = regex("\\bUCLM\\b",
                      ignore_case = T),
      replacement = "t.test"
    ) %>%
    str_replace_all(
      pattern = regex("\\bSKEW\\b",
                      ignore_case = T),
      replacement = "skewness"
    ) %>%
    str_replace_all(
      pattern = regex("\\bSTDDEV\\b",
                      ignore_case = T),
      replacement = "sd"
    ) %>%
    str_replace_all(
      pattern = regex("\\bSTD\\b",
                      ignore_case = T),
      replacement = "sd"
    ) %>%
    str_replace_all(
      pattern = regex("\\bN\\(([a-zA-z0-9._]+)\\)", ignore_case = T),
      replacement = "n()"
    ) %>%
    str_replace_all(
      pattern = regex("\\bMEAN\\b", ignore_case = T),
      replacement = "mean"
    ) %>%
    str_replace_all(
      pattern = regex("\\bMIN\\b", ignore_case = T),
      replacement = "min"
    ) %>%
    str_replace_all(
      pattern = regex("\\bMAX\\b", ignore_case = T),
      replacement = "max"
    ) %>%
    str_replace_all(
      pattern = regex("NMISS\\(([a-zA-z0-9._]+)\\)", ignore_case = T),
      replacement = "sum(is.na(\\1))"
    ) %>%
    str_replace_all(
      pattern = regex("\\bP([0-9]+)\\(([a-zA-z0-9._]+)\\)", ignore_case = T),
      replacement = "quantile(\\2, \\1/100)"
    )

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

  return(chaine)
}


#' Transform listes
#' @description remplace une liste SAS par son équivalent vectoriel R
#' @param chaine liste SAS au format {l1 l2 l3}

transform_list <- function(chaine){
  valeurs <- chaine %>%
    str_remove(pattern = "\\{") %>%
    str_remove(pattern = "\\}") %>%
    str_trim() %>%
    str_split(pattern = "\\s+") %>%
    unlist()

  valeurs_numeric <- suppressWarnings(as.numeric(valeurs))
  if(all(!is.na(valeurs_numeric))){
    valeurs <- valeurs_numeric
  }

  return(paste(list(valeurs), collapse = ", "))
}


#' transform path
#' @description change le chemin d'un fichier de façon compatible à la lecture dans R
#' En changeant \ en /
#' @param chaine chemin du fichier
transform_path <- function(chaine){
  return(gsub("\\", "/", chaine, fixed=TRUE))
}


