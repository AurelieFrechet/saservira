#' Decoupe code SAS
#'
#' @import stringr
#' @import dplyr
#'
#' @description identifie les blocs de procédures/commentaires/étapes data/etc,
#' les découpe et indique leur position dans le code d'origine
#'
#' @param code_sas : code SAS en entrée, non découpé par lignes
#'
#' @return liste d'élements : place, texte et id
#' place : localisation du code extrait
#' texte : code extrait
#' id : identifiant du code extrait, premier mot ou groupe de mot du bloc,
#' identifiant la procédure ou le commentaire, etc.
#' @export
#'
#' @examples
decouper_SAS <- function(code_sas) {
  # PROCEDURES : proc mot [...] run;/quit;
  locate_proc <- str_locate_all(
    code_sas,
    regex(pattern =   "(?=(^proc \\w+))[\\s\\S]*?(?<=(run;|quit;))",
          multiline = TRUE)
  )[[1]]
  match_proc  <- str_match_all(
    code_sas,
    regex(pattern =  "(?=(^proc \\w+))[\\s\\S]*?(?<=(run;|quit;))",
          multiline = TRUE)
  )[[1]]

  # ETAPES DATA : data [...] run;
  locate_data <- str_locate_all(code_sas,
                                regex(pattern   = "(?=(^data))[\\s\\S]*?(?<=(run;))",
                                      multiline = TRUE))[[1]]
  match_data  <- str_match_all(code_sas,
                               regex(pattern   = "(?=(^data))[\\s\\S]*?(?<=(run;))",
                                     multiline = TRUE))[[1]]

  # COMMENTAIRES 1 LIGNE
  locate_c1   <- str_locate_all(code_sas,
                                regex(pattern   = "(?=(^\\*)).*?(?<=\\n)",
                                      multiline = TRUE))[[1]]
  match_c1  <- str_match_all(code_sas,
                             regex(pattern   = "(?=(^\\*)).*?(?<=\\n)",
                                   multiline = TRUE))[[1]]

  # COMMENTAIRES MULTIGNES
  locate_c2   <- str_locate_all(code_sas,
                                regex(pattern   = "(?=(\\/\\*))[\\s\\S]*?(?<=(\\*\\/))",
                                      multiline = TRUE))[[1]]
  match_c2    <- str_match_all(code_sas,
                               regex(pattern   = "(?=(\\/\\*))[\\s\\S]*?(?<=(\\*\\/))",
                                     multiline = TRUE))[[1]]



  return(list(
    place = rbind(locate_proc,
                  locate_data,
                  locate_c1,
                  locate_c2),
    texte = c(match_proc[, 1],
              match_data[, 1],
              match_c1[, 1],
              match_c2[, 1]),
    id = c(match_proc[, 2],
           match_data[, 2],
           match_c1[, 2],
           match_c2[, 2])
  ))
}
