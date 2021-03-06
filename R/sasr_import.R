sasr_import <- function(code_sas) {
  code_net <- code_sas %>%
    str_remove(pattern = regex("proc\\s+import\\s+", ignore_case = T)) %>%
    str_remove(pattern = regex("run\\s*;", ignore_case = T)) %>%
    str_remove_all(pattern = ";") %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "=", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    decoupe_requete(
      requete = .,
      key_words = c("datafile",
                    "table",
                    "out",
                    "file",
                    "dbms",
                    "sheet",
                    "delimiter",
                    "getnames")
    )
}
