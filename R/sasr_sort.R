sasr_sort <- function(code_sas) {
   # Nettoyage et découpage de la procédure
   code_net <- code_sas %>%
      str_remove_all(pattern = regex("proc\\ssort\\s", ignore_case = T)) %>%
      str_remove_all(pattern = regex("run\\s*;", ignore_case = T)) %>%
      str_remove_all(pattern = ";") %>%
      str_replace_all(pattern = "\n", replacement = " ") %>%
      str_replace_all(pattern = "=", replacement = " ") %>%
      str_replace_all(pattern = "\\s+", replacement = " ") %>%
      decoupe_requete(requete = .,
                      key_words = c("data", "out", "by"))

   # DATA
   new_data <- code_net$text[code_net$kw == "data"]

   # OUT : Optionnel
   new_out <- code_net$text[code_net$kw == "out"]


   # BY
   script <- code_net$text[code_net$kw == "by"] %>%
      str_replace_all(pattern = "descending\\s(\\w+)",
                      replacement = "desc(\\1)") %>%
      str_replace_all(pattern = "\\s+",
                      replacement = ", ") %>%
      paste0(new_data, " %>% arrange(", ., ")")


   # Si option OUT écriture dans un objet
   if(!identical(new_out, character(0))){
      script <- paste(new_out, script, sep = " <- ")
   }

   return(script)

}
