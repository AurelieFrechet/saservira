library(stringr)
library(stringi)
library(dplyr)

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
