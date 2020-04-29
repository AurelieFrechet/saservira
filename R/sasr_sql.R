sql_dplyr_select <- function(select_clause) {
  code <-
    select_clause %>%
    str_split(pattern = ",") %>%
    unlist() %>%
    str_trim()

  nom_var <-
    str_extract(code,
                pattern = "(?<=as\\s).*")

  select_code <- nom_var %>%
    ifelse(is.na(.), code, .) %>%
    paste0("\"", ., "\"") %>%
    paste(., collapse = ", ") %>%
    paste0("select(", ., ")")

  fonction <- str_detect(code, "\\(")

  contenu <-
    str_extract(code,
                pattern = ".*(?=\\sas)")

  affectation <-
    ifelse(is.na(nom_var), NA,
           paste(nom_var, contenu, sep = " = ")) %>%
    {
      .[!is.na(.)]
    } %>%
    paste(., collapse = ", ")

  if (all(fonction)) {
    # Que des fonctions
    return_code <- affectation %>%
      paste0("summarize(", ., ")")

  } else {
    if (all(is.na(contenu))) {
      # Pas d'affection de variable
      return_code <- select_code

    } else{
      # Creation de variable
      return_code <- affectation %>%
        paste0("mutate(", ., ") %>%\n\t", select_code)
    }
  }

  return(return_code)
}


#' sql_to_dplyr
#' @include decoupe.R
#' @import dplyr
#' @import stringr
#' @param code_sql : chaine de charactère code SQL
#'
#' @return chaine de charactere
#' @export
#'
#' @examples
sql_to_dplyr <- function(code_sql) {
  # Déclaration des variables
  nom <- colonne <- NULL
  dplyr_mutate  <- NA
  dplyr_select  <- NA
  dplyr_data    <- NA
  dplyr_filter  <- NA
  dplyr_arrange <- NA
  dplyr_groupby <- NA

  # Initialisation
  sentence <- decoupe_requete(code_sql,
                              key_words = c("select",
                                            "from",
                                            "where",
                                            "order by",
                                            "group by",
                                            "limit"))



  # Partie SELECT ----
  if (sentence["select"] != "*") {
    dplyr_select <- sql_dplyr_select(sentence["select"])
  }

  # Partie FROM ----
  from_vector <-  sentence["from"] %>%
    str_split(pattern = ",") %>%
    unlist()

  if (length(from_vector) > 1) {
    # TODO les jointures impropres
  } else {
    dplyr_data <- from_vector
  }


  # Partie WHERE ----
  if (!is.na(sentence["where"])) {
    dplyr_filter <- sentence["where"] %>%

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
      str_replace_all(pattern = "\\s?not\\s?",  replacement = " !") %>%

      # Mise en fonction
      paste0("filter(", ., ")")
  }

  # TODO : Partie Order by ----
  if (!is.na(sentence["order by"])) {
    dplyr_arrange <- sentence["order by"] %>%
      paste0("arrange(", . ,")")
  }


  # TODO : Partie Groupe by ----
  if (!is.na(sentence["group by"])) {
    dplyr_groupby <- sentence["group by"]%>%
      paste0("group_by(", . ,")")
  }


  # Return
  requete_dplyr <- c(dplyr_data,
                     dplyr_mutate,
                     dplyr_select,
                     dplyr_filter,
                     dplyr_arrange,
                     dplyr_groupby) %>%
    {
      .[!is.na(.)]
    } %>%
    paste(., collapse = " %>%\n\t")

  return(requete_dplyr)

}

#' sasr_sql
#' @include decoupe.R
#' @param code_sas code SAS balisé de proc sql; quit;
#'
#' @return la même requeteen R library dplyr
#' @export
#'
#' @examples
sasr_sql <- function(code_sas) {
  # Séparer les différentes requêtes ----
  requetes <- code_sas %>%
    str_remove(pattern = "proc sql;") %>%
    str_remove(pattern = "quit;") %>%
    str_split(pattern = ";") %>%
    unlist() %>%
    str_replace_all(pattern = "\n", " ") %>%
    str_trim() %>%
    {
      .[-which(. == "")]
    }

  # Mise en fonction dplyr pour chaque requete
  requetes_dplyr <- lapply(requetes, sql_to_dplyr) %>%
    unlist() %>%
    paste(., collapse = "\n")

  return(requetes_dplyr)

}
