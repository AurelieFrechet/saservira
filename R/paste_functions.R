paste_function <- function(fonction, contenu){
  paste0(fonction, "(", contenu, ")")
}

#' paste_str
#' @description Colle la fonction str() autour d'un string
#' @param string chaine de caractere contenu de la fonction
paste_str <- function(string) {
  paste_function("str", string)
}

paste_summary <- function(string){
  paste_function("summary", string)
}
