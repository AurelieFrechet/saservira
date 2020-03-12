sasr_sql <- function(code_sas) {
  # Séparer les différentes requêtes ----
  requetes <- code_sas %>%
    str_replace(pattern = "proc sql;", replacement = "") %>%
    str_replace(pattern = "quit;", replacement = "") %>%
    str_split(pattern = ";") %>%
    unlist() %>%
    {
      .[-which(str_detect(., "^\n$"))]
    }

  # Couper au niveau des mots clés ----
  key_words <- c("select",
                 "from",
                 "where",
                 "order by",
                 "group by",
                 "limit")
  pattern_kw <- paste(
    paste0("(?=", key_words, ")"),
    collapse = "|")

  requetes_list <- lapply(
    X = requetes,
    FUN = function(x) {
      str_split(string = x,
                pattern = pattern_kw)[[1]] %>%
        {
          .[-which(str_detect(., "^\n"))]
        } %>%
        str_replace(pattern = "\n$", replacement = "") %>%
        str_trim()
    }
  )

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
