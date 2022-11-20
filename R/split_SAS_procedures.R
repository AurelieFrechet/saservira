#' Title
#'
#' @import stringr
#' @import magrittr
#' @param text
#'
#' @return
#' @export
#'
#' @examples
split_SAS_procedures <- function(text) {
  # Clean Text
    text %>%
    tolower() %>%
    stringr::str_squish() %>%
    stringr::str_replace_all(pattern = regex("run\\s?;",
                                    ignore_case = TRUE),
                    replacement = "run;") %>%
    stringr::str_replace_all(pattern = regex("quit\\s?;",
                                    ignore_case = TRUE),
                    replacement = "quit;") %>%
    strsplit('(?=\\s(proc \\w+))|(?<=run;)|(?=\\s(data \\w+))|(?<=quit;)',
             perl = T) %>%
    unlist() %>%
    stringr::str_squish() %>%
    {.[!.==""]} %>%
    {.[!.=="quit;"]}
}
