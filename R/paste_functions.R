paste_function <- function(fonction, contenu){
  paste0(fonction, "(", contenu, ")")
}

#' paste_str
#' @description Colle la fonction str() autour d'un contenu
#' @param contenu chaine de caractere contenu de la fonction
paste_str <- function(contenu) {
  paste_function("str", contenu)
}

paste_summary <- function(contenu){
  paste_function("summary", contenu)
}

paste_table <- function(contenu){
  paste_function("table", contenu)
}
