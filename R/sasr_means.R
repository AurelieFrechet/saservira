sasr_means <- function(code_sas){
  code_net <- code_sas %>%
    str_remove_all(pattern  = regex("proc\\smeans\\s", ignore_case = T)) %>%
    str_remove_all(pattern  = regex("run\\s*;", ignore_case = T)) %>%
    str_remove_all(pattern  = ";") %>%
    str_replace_all(pattern = "\n",   replacement = " ") %>%
    str_replace_all(pattern = "=",    replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    decoupe_requete(requete = .,
                    key_words = c("data",
                                  "by",
                                  "class",
                                  "freq",
                                  "id",
                                  "output",
                                  "types",
                                  "var",
                                  "weight",
                                  "ways"))
}
