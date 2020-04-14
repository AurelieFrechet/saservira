globalVariables(c("."))

reecriture <- function(id, code) {
  switch(id,
         "proc sql" = {
           sasr_sql(code)
         })
}


#' Transcripteur
#'
#' @import dplyr
#' @description transcrit du code SAS en R
#'
#' @param input fichier SAS
#' @param output fichier R
#'
#' @export
#'
transcripteur <- function(input, output) {
  code_sas <- readLines(input, encoding = "UTF-8", warn=FALSE) %>%
    paste(., collapse = "\n") %>%
    tolower() %>%
    str_replace_all(pattern = "run\\s?;" , replacement = "run;")%>%
    str_replace_all(pattern = "quit\\s?;", replacement = "quit;")


  code_decoupe <- decouper_SAS(code_sas)

  traduction <- lapply(
    X = 1:length(code_decoupe$id),
    FUN = function(i) {
      reecriture(id   = code_decoupe$id[i],
                 code = code_decoupe$texte[i])
    }
  )
}
