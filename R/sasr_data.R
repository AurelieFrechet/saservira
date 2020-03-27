sasr_data <- function(code_sas){
  # Déclaration des variables
  data_table        <- NA
  set_table         <- NA
  variables_brut    <- NA
  length_variables  <- NA
  input_variabless  <- NA
  datalines         <- NA
  keep_variables    <- NA
  drop_variables    <- NA
  where             <- NA

  # data lib.table;
  data_table <- str_match_all(code_sas,
                              regex(pattern   = "^data (\\w+\\.)?(\\w+) ?;",
                                    multiline = TRUE))[[1]]

  # <set lib.table;>
  set_table <- str_match_all(code_sas,
                          regex(pattern   = "^set (\\w+\\.)?(\\w+) ?;",
                                multiline = TRUE))[[1]]

  # var = contenu;
  variables_brut <- str_match_all(code_sas,
                                  regex(pattern   = "^(\\w+) ?= ?(\\w+) ?;",
                                        multiline = TRUE))[[1]]

  # <length var format;>
  length_variables
  #<input var contenu;>
  input_variabless
  # datalines;
  # matrice donnees;
  datalines

  keep_variables
  drop_variables
  where


}


