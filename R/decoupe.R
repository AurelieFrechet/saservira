#' Decoupe code SAS
#'
#' @import stringr
#' @import stringi
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
    regex(pattern =   "(?=(proc \\w+))[\\s\\S]*?(?<=(run;|quit;))",
          multiline = TRUE,
          ignore_case = TRUE)
  )[[1]]
  match_proc  <- str_match_all(
    code_sas,
    regex(pattern =  "(?=(proc \\w+))[\\s\\S]*?(?<=(run;|quit;))",
          multiline = TRUE,
          ignore_case = TRUE)
  )[[1]]

  # ETAPES DATA : data [...] run;
  locate_data <- str_locate_all(code_sas,
                                regex(pattern   = "(?=(^data))[\\s\\S]*?(?<=(run;))",
                                      multiline = TRUE,
                                      ignore_case = TRUE))[[1]]
  match_data  <- str_match_all(code_sas,
                               regex(pattern   = "(?=(^data))[\\s\\S]*?(?<=(run;))",
                                     multiline = TRUE,
                                     ignore_case = TRUE))[[1]]

  # COMMENTAIRES 1 LIGNE
  locate_c1   <- str_locate_all(code_sas,
                                regex(pattern   = "(?=(^(\\s)*?\\*)).*?(?=\\n)",
                                      multiline = TRUE))[[1]]
  match_c1  <- str_match_all(code_sas,
                             regex(pattern   = "(?=(^(\\s)*?\\*)).*?(?=\\n)",
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
    id = tolower(
      str_trim(c(match_proc[, 2],
           match_data[, 2],
           match_c1[, 2],
           match_c2[, 2])))
  ))
}

#' decoupe_requete
#' @import stringr
#' @import stringi
#' @import dplyr
#' @description lit une requete sql et renvoie une data.frame avec les mots clés (kw)
#' et les valeurs associées (sentence)
#' @param requete une seule requete sql
#' @param key_words : mots clés de découpe (select, from, etc)
#'
#' @return vecteur nommé des blocs, le nom associé correspond aux mots clés
#' @export
#'
#' @examples
decoupe_requete <- function(requete, key_words){
  # Mise sous forme de mots :
  key_words <- paste0("\\b", key_words, "\\b")
  # Definition des mots clés
  pattern_kw <- paste(paste0("(?=", key_words, ")"),
                      collapse = "|")

  # Decoupe
  sentence <- str_split(string = requete,
                        pattern = regex(pattern_kw, ignore_case = T))[[1]] %>%
    str_trim() %>%
    {
      .[!(. == "")]
    }


  # Identification
  kw <- str_extract(string = tolower(sentence),
                    pattern = paste(key_words, collapse = "|"))

  # Nettoyage
  kw_pattern <- paste(key_words, collapse = "|")
  sentence <- sentence %>%
    str_replace_all(pattern = regex(kw_pattern, ignore_case = T),
                    replacement = "") %>%
    str_trim()

  # Messages d'erreur
  if(all(is.na(kw))){
    message("Requete does not contain key words")
    return(NULL)
  }

  return(list(kw = kw, text = sentence))
}
