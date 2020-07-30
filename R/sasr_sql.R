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
  code <- select_clause %>%
    str_split(pattern = ",") %>%
    unlist() %>%
    str_trim %>%
    transform_functions()

  attribution <- code %>%
    str_split(pattern = "\\sinto\\s?:\\s?|\\sas\\s|\\s")

  attribution <- do.call(rbind, attribution) %>%
    as.data.frame(stringsAsFactors = FALSE)

  if(ncol(attribution) == 2){
    noms_var = attribution[, 2]
    contenu  = ifelse(attribution[, 1] == attribution[, 2],
                      NA,
                      attribution[, 1])
  } else {
    noms_var = attribution[, 1]
    contenu  = rep(NA, length(noms_var))
  }

  # Detection de contenu
  is_create <- ifelse(is.na(contenu), FALSE, TRUE)

  # Détection de fonctions
  # TODO : détection des fonctions d'aggregation uniquement :
  # - AVG()
  # - COUNT()
  # - MAX()
  # - MIN()
  # - SUM()
  is_function <- str_detect(string = code, pattern = "\\(")


  # Préparation du select général
  select_code <- noms_var %>%
    # paste0("\"", ., "\"") %>%
    paste(., collapse = ", ") %>%
    paste0("select(", ., ")")

  # Affectation des noms de variables à leur contenu
  affectation <-
    ifelse(is.na(contenu), NA,
           paste(noms_var, contenu, sep = " = ")) %>%
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
      return_code <- select_code %>%
        paste0(., " %>% \n\tdistinct()")
    } else {
      ## SI ne contient que des fonctions d'aggregation
      if (all(is_function)) {
        if(any(is_create)){
          return_code <- affectation %>%
            paste0("summarize(", ., ")")
        } else {
          return_code <- noms_var %>%
            paste(., collapse = ", ") %>%
            paste0("summarize(", ., ")")
        }

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
  affectation   <- NA
  dplyr_mutate  <- NA
  dplyr_select  <- NA
  dplyr_data    <- NA
  dplyr_filter  <- NA
  dplyr_arrange <- NA
  dplyr_groupby <- NA
  dplyr_join    <- NA
  affectation   <- NA

  # Initialisation
  sentence <- decoupe_requete(code_sql,
                              key_words = c("select",
                                            "from",
                                            "where",
                                            "order by",
                                            "having",
                                            "group by",
                                            "left join",
                                            "right join",
                                            "inner join",
                                            "full join",
                                            "create table"))


  # Partie GROUP BY ----
  if (any(sentence$kw == "group by")) {

    # Soustraction des var du group by au select
    var_groupby <- sentence$text[(sentence$kw == "group by")] %>%
      str_split(pattern = ',') %>%
      unlist() %>%
      str_trim()


    var_select <- sentence$text[(sentence$kw == "select")] %>%
      str_split(pattern = ',') %>%
      unlist() %>%
      str_trim()


    sentence$text[(sentence$kw == "select")] <-
      setdiff(var_select, var_groupby) %>%
      paste(., collapse = ", ")


    dplyr_groupby <- var_groupby %>%
      paste0("group_by(", . ,")")
  }

  # Partie SELECT ----
  if (sentence$text[(sentence$kw == "select")] != "*"
      & any(sentence$kw == "select")) {
    # Détecter les prefixes et les supprimer
    # Note : choix de tout supprimer peut-être à revoire plus tard
    dplyr_select <- sentence$text[(sentence$kw == "select")] %>%
      str_remove_all(pattern = "\\w+\\.") %>%
      sql_dplyr_select()
  }

  # Partie FROM ----
  # TODO : Détecter les abréviations FROM table_machin t1
  from_vector <- sentence$text[(sentence$kw == "from")] %>%
    str_split(pattern = ",") %>%
    unlist()

  if (length(from_vector) > 1) {
    # TODO les jointures impropres
  } else {
    dplyr_data <- from_vector
  }


  # Partie WHERE ----
  if (any(sentence$kw == "where")) {
    dplyr_filter <- sentence$text[(sentence$kw == "where")] %>%
      transform_conditions() %>%
      paste0("filter(", ., ")")
  }

  # Partie HAVING ----
  if (any(sentence$kw == "having")) {
    dplyr_filter <- sentence$text[(sentence$kw == "having")] %>%
      transform_conditions() %>%
      paste0("filter(", ., ")")
  }

  # Partie Order by ----
  if (any(sentence$kw == "order by")) {
    dplyr_arrange <- sentence$text[(sentence$kw == "order by")] %>%
      str_replace_all(pattern = regex("([\\S]+)\\sdesc", ignore_case = T),
                      replacement = "-\\1") %>%
      paste0("arrange(", . ,")")
  }

  # Jointures ----
  if (TRUE){
   # Possible d'avoir plusieurs jointures
  }

  # CREATE TABLE ----
  if (any(sentence$kw == "create table")){
    # CAS CREATE TABLE ______ AS
    lecture <- sentence$text[(sentence$kw == "create table")] %>%
          str_match(pattern = regex("([\\S]+)\\s(as|like)(\\s[\\S]+)?"))

    nom_table <-  lecture[ , 2]
    table_like <- lecture[, 4]

    # CAS CREATE TABLE ______ LIKE

    # CAS CREATE TABLE ______ ()
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
    str_remove(pattern = regex("proc\\s+sql\\s*;", ignore_case = T)) %>%
    str_remove(pattern = regex("quit\\s*;", ignore_case = T)) %>%
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
