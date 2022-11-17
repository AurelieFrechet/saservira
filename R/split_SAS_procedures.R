split_SAS_procedures <- function(text) {
  # Clean Text
  text %>%
    tolower() %>%
    str_squish() %>%
    str_replace_all(pattern = regex("run\\s?;",
                                    ignore_case = TRUE),
                    replacement = "run;") %>%
    str_replace_all(pattern = regex("quit\\s?;",
                                    ignore_case = TRUE),
                    replacement = "quit;") %>%
    strsplit('(?=\\s(proc \\w+))|(?<=run;)|(?=\\s(data \\w+))|(?<=quit;)',
             perl = T) %>%
    unlist() %>%
    str_squish() %>%
    {.[!.==""]}
}
