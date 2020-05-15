globalVariables(c("."))

reecriture <- function(id, code) {
  switch(id,
         "proc sql" = {
           sasr_sql(code)
         },
         "proc contents" = {
           sasr_contents(code)
         })
}


#' Transcripteur
#' @include decoupe.R
#' @import dplyr
#' @description traduit du code SAS en R
#'
#' @param input fichier SAS
#' @param output fichier R
#'
#' @export
#'
traducteur <- function(code_sas) {
  # code_sas <- readLines(input, encoding = "UTF-8", warn=FALSE) %>%
  #   paste(., collapse = "\n") %>%
  code_sas <-  code_sas %>%
    str_replace_all(pattern = regex("run\\s?;",
                                    ignore_case = TRUE),
                    replacement = "run;") %>%
    str_replace_all(pattern = regex("quit\\s?;",
                                    ignore_case = TRUE),
                    replacement = "quit;")

  code_decoupe <- decouper_SAS(code_sas)

  if (length(code_decoupe$id) > 0) {
    code_decoupe$traduction <- lapply(
      X = 1:length(code_decoupe$id),
      FUN = function(i) {
        reecriture(id   = code_decoupe$id[i],
                   code = code_decoupe$texte[i])
      }
    ) %>% unlist()


    stri_sub_all(str = code_sas, code_decoupe$place) <-
      code_decoupe$traduction
  }


  return(code_sas)

}
