# Difficulté de la proc means : peut être traduite de diverses façon en fonction ds aguments.
# On choisit une traduction via dplyr:summarize quand les indicateurs sont spécifiés,
# et via summary lors qu'ils ne sonts pas présents
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
                                  "format",
                                  "freq",
                                  "id",
                                  "output",
                                  "types",
                                  "var",
                                  "weight",
                                  "ways"))

  # Cas OUTPUT -----------------------------------------------------------------
  # Défini nom de la table de sortie et les noms de variables crées
  means_output <- code_net$text[(code_net$kw == "output")]
  output <- !is.null(means_output)

  # Découpe DATA=data <option> -------------------------------------------------
  means_data <- code_net$text[(code_net$kw == "data")] %>%
    str_split("\\s+") %>%
    unlist()
  # le premier mot correspond aux données,
  # les suivants aux indicateurs sélectionnés
  means_indic <- means_data[-1] %>%
    { .[!. == "noprint"] } # Exclure les options type noprint

  # TODO : si pas d'indicateurs mais pas de output
  if(identical(means_indic, character(0))) {
    # Si OUTPUT : means_indic <- output_indic
    # Si BY ou Class : means_indic <- "N MEAN STD MIN MAX
    # Sinon : summary
  }

  dplyr_data  <- means_data[1]

  # Sélection des variables ----------------------------------------------------
  means_var <- code_net$text[(code_net$kw == "var")]
  #  distinguer s'il y a une ou plusieurs variables
  nb_vars <- str_count(means_var, pattern = "[A-Za-z0-9._]+") #
  if (nb_vars == 1) {
    dplyr_summarize <- paste0(means_indic, "(", means_var, ")") %>%
      transform_functions() %>%  # réécrire les indicateurs de façon compatibles
      paste(., collapse = ", ") %>%
      paste0("summarize(", ., ")")


  } else{
    # selection des variables
    dplyr_select <- means_var %>%
      str_replace_all(pattern = "-", replacement = ":") %>%
      str_replace_all(pattern = "\\s+", replacement = ", ") %>%
      paste0("select(", ., ")")

    # summarize_all
    dplyr_summarize <- means_indic %>%
      paste(., ., sep = "=") %>%
      paste(., collapse = ", ") %>%
      paste0("summarize_all(list(", ., "))") %>%
      paste(dplyr_select, ., sep = " %>%\n\t")
  }

  # Regroument BY et CLASS -----------------------------------------------------
  dplyr_groupby <- NA
  if (any(code_net$kw == "by") | any(code_net$kw == "class")) {
  dplyr_groupby <- paste(code_net$text[(code_net$kw == "by")],
                         code_net$text[(code_net$kw == "class")]) %>%
    str_trim() %>%
    str_replace_all(pattern = "\\s+", replacement = ", ") %>%
    paste0("group_by(", ., ")")
  }


  # Composition de la sortie
  requete_dplyr <- c(dplyr_data,
                     dplyr_groupby,
                     dplyr_summarize) %>%
    {
      .[!is.na(.)]
    } %>%
    paste(., collapse = " %>%\n\t")

 return(requete_dplyr)
}
