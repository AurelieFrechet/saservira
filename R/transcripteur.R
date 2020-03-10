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
  code_sas <- readLines(input) %>%
    paste(., collapse = "\n")
}

input = "exemple_sas"
