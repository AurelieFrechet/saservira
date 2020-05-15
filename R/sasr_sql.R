# Merci à Nolwenn Lannuel :)



#' @include utils.R
sql_dplyr_select <- function(select_clause) {
  # Détection du ALL
  is_all       <- select_clause == "*"
  contains_all <-
    str_detect(string = select_clause, pattern = "\\*")

  # Détection du DISTINCT
  is_distinct  <- str_detect(string = select_clause,
                             pattern = regex("distinct", ignore_case = T))

  # Découpage de la clause par la virgule
  code <-
    select_clause %>%
    str_split(pattern = ",") %>%
    unlist() %>%
    str_trim()

  # Détection de création de variable
  is_create   <- str_detect(string = code, pattern = "\\sas\\s")

  # Détection de fonctions
  is_function <- str_detect(string = code, pattern = "\\(")


  # Extraction du nom des nouvelles variables
  nom_var <-
    str_extract(code,
                pattern = "(?<=as\\s).*")
  # Extraction du contenu des nouvelles variables
  contenu <-
    str_extract(code,
                pattern = ".*(?=\\sas)") %>%
    transform_functions()

  # Préparation du select général
  select_code <- nom_var %>%
    ifelse(is.na(.), code, .) %>%
    paste0("\"", ., "\"") %>%
    paste(., collapse = ", ") %>%
    paste0("select(", ., ")")

  # Affectation des noms de variables à leur contenu
  affectation <-
    ifelse(is.na(nom_var), NA,
           paste(nom_var, contenu, sep = " = ")) %>%
    {
      .[!is.na(.)]
    } %>%
    paste(., collapse = ", ")


  ## SI ALL
  if (is_all) {
    return_code <- NULL
  } else {
    ## SI DISTINCT
    if (is_distinct) {
      #TODO
    } else {
      ## SI ne contient que des fonctions
      if (all(is_function)) {
        return_code <- affectation %>%
          paste0("summarize(", ., ")")
      } else {
        ## SI ne contient que des créations de variables
        if (all(is_create)) {
          return_code <- affectation %>%
            paste0("transmute(", ., ")")
        } else {
          # Si contient ALL
          if (contains_all & any(is_create)) {
            return_code <- affectation %>%
              paste0("mutate(", ., ")")
          } else {
            # SI Extraction pure
            if (all(is.na(contenu))) {
              # Pas d'affection de variable
              return_code <- select_code

            } else{
              # Creation de variable
              return_code <- affectation %>%
                paste0("mutate(", ., ") %>%\n\t", select_code)
            }

          }
        }

      }
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
                                            "having",
                                            "group by",
                                            "limit"))


  # Partie Groupe by ----
  if (!is.na(sentence["group by"])) {

    # Soustraction des var du group by au select
    var_groupby <- sentence["group by"]%>%
      str_split(pattern = ',') %>%
      unlist() %>%
      str_trim()


    var_select <- sentence["select"]%>%
      str_split(pattern = ',') %>%
      unlist() %>%
      str_trim()


    sentence["select"] <- setdiff(var_select, var_groupby) %>%
      paste(., collapse = ", ")


    dplyr_groupby <- var_groupby %>%
      paste0("group_by(", . ,")")
  }

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
      transform_conditions() %>%
      paste0("filter(", ., ")")
  }

  # Partie HAVING ----
  if (!is.na(sentence["having"])) {
    dplyr_filter <- sentence["having"] %>%
      transform_conditions() %>%
      paste0("filter(", ., ")")
  }

  # Partie Order by ----
  if (!is.na(sentence["order by"])) {
    dplyr_arrange <- sentence["order by"] %>%
      str_replace_all(pattern = regex("([\\S]+)\\sdesc", ignore_case = T),
                      replacement = "-\\1") %>%
      paste0("arrange(", . ,")")
  }





  # Return
  requete_dplyr <- c(dplyr_data,
                     dplyr_groupby,
                     dplyr_mutate,
                     dplyr_select,
                     dplyr_filter,
                     dplyr_arrange) %>%
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
