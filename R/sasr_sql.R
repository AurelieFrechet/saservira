#' lecture_sql
#' @description lit une requete sql et renvoie une data.frame avec les mots clés (kw)
#' et les valeurs associées (sentence)
#' @param requete une seule requete sql
#'
#' @return une data.frame 2 col
#' @export
#'
#' @examples
decoupe_requete <- function(requete, key_words){
  # Definition des mots clés
  pattern_kw <- paste(
    paste0("(?=", key_words, ")"),
    collapse = "|")

  # Decoupe
  sentence <- str_split(string = requete,
                        pattern = pattern_kw)[[1]] %>%
    {
      .[-which(str_detect(., "^\n"))]
    } %>%
    str_remove(pattern = "\n$") %>%
    str_trim()

  # Identification
  kw <- str_extract(string = sentence,
                    pattern = paste(key_words, collapse = "|"))

  # Nettoyage
  sentence <- sentence %>%
    str_remove(pattern = paste(key_words, collapse = "|")) %>%
    str_trim()

  names(sentence) <-kw
  return(sentence)
}

# TODO : Créer un interpréteur de data.frame
sql_to_dplyr <- function(sentence){
  if(sentence["select"] == "*"){

  }else{
    select_matrix <- sentence["select"] %>%
      str_split(pattern = ",") %>%
      unlist() %>%
      str_trim() %>%
      str_match_all(pattern = "([a-zA-Z0-9.()]+)(\\sas\\s)?([a-zA-Z0-9]+)?") %>%
      do.call(rbind, .)

    select_df <-
      data.frame(colonne = select_matrix[, 2],
                 nom     = select_matrix[, 4],
                 stringsAsFactors = FALSE) %>%
      mutate(select = ifelse(is.na(nom), colonne, nom),
             mutate = ifelse(is.na(nom), NA, paste(nom, colonne, sep = " = ")))

    }
}

sasr_sql <- function(code_sas) {
  # Séparer les différentes requêtes ----
  requetes <- code_sas %>%
    str_remove(pattern = "proc sql;") %>%
    str_remove(pattern = "quit;") %>%
    str_split(pattern = ";") %>%
    unlist() %>%
    {
      .[-which(str_detect(., "^\n$"))]
    }

  # Couper au niveau des mots clés ----

  requetes_list <- lapply(requetes, decoupe_requete,   key_words = c("select",
                                                                  "from",
                                                                  "where",
                                                                  "order by",
                                                                  "group by",
                                                                  "limit"))

  # SELECT
  sql_select <-
    str_extract_all(
      code_sas,
      regex(pattern   = paste(
        "select [a-zA-Z0-9.()]+(\\sas [a-zA-Z0-9]+)?(\\s?,\\s?[a-zA-Z0-9.()]+(\\sas [a-zA-Z0-9]+)?)*",
        "select \\*", sep ="|"),
            multiline = TRUE))[[1]] %>%
    str_replace(pattern = "select ", replacement = "") %>%
    str_split(pattern = "\\s?,\\s?")

  # FROM
  sql_from <-
    str_extract_all(
      code_sas,
      regex(pattern   = "from [a-zA-Z0-9.]+(\\s?,\\s?[a-zA-Z0-9.]+)*",
            multiline = TRUE))[[1]] %>%
    str_replace(pattern = "from ", replacement = "") %>%
    str_split(pattern = "\\s?,\\s?")

  # WHERE
  sql_where <-
    str_extract_all(
      code_sas,
      regex(pattern   = "(?=where)[\\s\\S]*?(?=(;|order by|group by))",
            multiline = TRUE))[[1]] %>%
    str_replace(pattern = "where ", replacement = "") %>%
    as.list()

  # GROUP BY
  sql_group_by <-
    str_extract_all(
      code_sas,
      regex(pattern   = "group by [a-zA-Z0-9.]+(\\s?,\\s?[a-zA-Z0-9.]+)*",
            multiline = TRUE))[[1]] %>%
    str_replace(pattern = "group by ", replacement = "")  %>%
    str_split(pattern = "\\s?,\\s?")

  # ORDER BY
  sql_order_by <-
    str_extract_all(
      code_sas,
      regex(pattern   = "order by [a-zA-Z0-9.]+(\\sdesc|\\sasc)?(\\s?,\\s?[a-zA-Z0-9.]+(\\sdesc|\\sasc)?)*",
            multiline = TRUE))[[1]] %>%
    str_replace(pattern = "order by ", replacement = "") %>%
    str_split(pattern = "\\s?,\\s?")


  "[a-zA-Z0-9.\\s]+\s?(=|<|>|<>|<=|>=)\s?[a-zA-Z0-9.\"']+|[a-zA-Z0-9.\\s]+\s(is not|is)\snull|[a-zA-Z0-9.\\s]+\sbetween \w+ and \w+"

  }
