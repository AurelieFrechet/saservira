

#' paste_str
#' @description Colle la fonction str() autour d'un string
#' @param string
#'
#' @return
#'
#' @examples
paste_str <- function(string) {
  paste0("str(", string, ")")
}


#' sasr_contents
#' @import stringr
#' @param code_sas
#' @description transformation de la proc contents en code R equivalent
#' @return code R equivalent de la proc contents avec les arguments
#' @export
#'
sasr_contents <- function(code_sas) {
  data <- str_match(string = code_sas,
                    pattern = "data\\s?=\\s?([0-9a-zA-Z.]+)")

  code_r <- paste_str(data[, 2])

  return(code_r)
}
