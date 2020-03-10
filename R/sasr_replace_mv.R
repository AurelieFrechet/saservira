sasr_replace_mv <- function(mv_nom, mv_contenu, code_sas){


  leftover <- str_count(code_sas, pattern = paste0("&", mv_nom))

  code_sas %>%
    str_replace_all(pattern = "&&", replacement = "&")

  while(leftover > 0){

  }
}

mv_nom = "nomTab"
mv_contenu = "clients"
code_sas = "&nomTab &nomTab. &nomTab1 &nomTab.1 &nomBib.&nomTab "
