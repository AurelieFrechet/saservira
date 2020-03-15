sql_to_dplyr <- function(sentence) {
  # Initialisation
  dplyr_mutate <- NA
  dplyr_select <- NA
  dplyr_data   <- NA
  dplyr_filter <- NA

  # Partie SELECT ----
  if (sentence["select"] != "*") {
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

    lignes_mutate <-  select_df %>%
      filter(!is.na(mutate)) %>%
      select(mutate)
    if (nrow(lignes_mutate) > 0) {
      dplyr_mutate <- paste("mutate(",
                            paste(lignes_mutate, collapse = ","), ")",
                            sep = "")
    }

    dplyr_select <- paste("select(",
                          paste(select_df$select, collapse = ","),
                          ")",
                          sep = "")
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
      str_replace_all(pattern = "=",  replacement = "==") %>%
      str_replace_all(pattern = "ge", replacement = ">=") %>%
      str_replace_all(pattern = "le", replacement = "<=") %>%
      str_replace_all(pattern = "<>", replacement = "!=") %>%


      # Remplacement IN
      str_replace(pattern = "([a-zA-Z0-9.]+)\\sin\\s([a-zA-Z0-9,()]+)",
                  replacement = "\\1 %in% c\\2") %>%

      # Remplacement BETWEEN
      str_replace(pattern = "([a-zA-Z0-9.]+)\\sbetween\\s(\\w+)\\sand\\s(\\w+)",
                  replacement = "between(\\1, \\2, \\3)") %>%

      # Remplacement LIKES
      # TODO

      # Remplacement and et or
      str_replace_all(pattern = "and",  replacement = "&") %>%
      str_replace_all(pattern = "or",   replacement = "|") %>%
      str_replace_all(pattern = "not",  replacement = "!") %>%

      # Mise en fonction
      paste0("filter(", ., ")")
  }

  # Return
  requete_dplyr <- c(dplyr_data,
                     dplyr_mutate,
                     dplyr_select,
                     dplyr_filter) %>%
    {
      .[!is.na(.)]
    } %>%
    paste(., collapse = " %>% ")

  return(requete_dplyr)

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

  requetes_dplyr <- lapply(requetes_list, sql_to_dplyr)

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
