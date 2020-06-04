#' sasr_libname
#' @description transformation l'un libname en une affectation
#' d'un chemin dans un objet R : Convertit LIBNAME nom chemin en nom <- chemin
#' @param code_sas code en entrée contenant le libname
#'
#' @return expression R associée
#' @export
#'
#' @examples
sasr_libname <- function(code_sas) {
  lib_match <- code_sas %>%
    # Transformation des \ en /
    transform_path() %>%
    # Identification du nom de la librarie et du chemin assosié
    str_match(pattern = "libname (\\w+) ([\"'][\\S]+[\"'])")

  # Gestion des options ?

  nom    <- lib_match[, 2]
  chemin <- lib_match[, 3]

  return(paste(nom, "<-", chemin))

}
