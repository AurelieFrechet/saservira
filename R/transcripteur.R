#' Transcripteur
#'
#' @description transcrit du code SAS en R
#'
#' @param input fichier SAS
#' @param output fichier R
#'
#' @export
#'
transcripteur <- function(input, output){
  code_sas <- readLines(input, encoding = "UTF-8") %>%
    paste(., collapse = "\n")

  code_decoupe <- decouper_SAS(code_sas)
}

# input = "exemple_sas"
