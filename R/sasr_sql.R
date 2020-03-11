sasr_sql <- function(code_sas){
  # SELECT
  sql_select <-
    str_extract_all(
      code_sas,
      regex(pattern   = "select [a-zA-Z0-9.]+(\\s?,\\s?[a-zA-Z0-9.]+)*|select \\*",
            multiline = TRUE))[[1]] %>%
    str_replace(pattern = "select ", replacement = "")

  # FROM
  sql_from <-
    str_extract_all(
      code_sas,
      regex(pattern   = "from [a-zA-Z0-9.]+(\\s?,\\s?[a-zA-Z0-9.]+)*",
            multiline = TRUE))[[1]] %>%
    str_replace(pattern = "from ", replacement = "")

  # WHERE
  sql_where <-
    str_extract_all(
      code_sas,
      regex(pattern   = "where ([a-zA-Z0-9.\\s]+(=|is not|is))*\\s?\\w+",
            multiline = TRUE))[[1]] %>%
    str_replace(pattern = "where ", replacement = "") %>%
    str_split(pattern = "\\s?(and|or)\\s?")

  # GROUP BY
  sql_group_by <-
    str_extract_all(
      code_sas,
      regex(pattern   = "group by [a-zA-Z0-9.]+(\\s?,\\s?[a-zA-Z0-9.]+)*",
            multiline = TRUE))[[1]] %>%
    str_replace(pattern = "group by ", replacement = "")

  # ORDER BY
  sql_order_by <-
    str_extract_all(
      code_sas,
      regex(pattern   = "order by [a-zA-Z0-9.]+(\\sdescending)?(\\s?,\\s?[a-zA-Z0-9.]+(\\sdescending)?)*",
            multiline = TRUE))[[1]] %>%
    str_replace(pattern = "order by ", replacement = "")


  }
