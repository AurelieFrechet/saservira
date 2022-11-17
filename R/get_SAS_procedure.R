#' get SAS procedures
#' @import stringr
#' @import magrittr
#' @description read SAS code, identify procedures and extract their contents
#' @param text SAS code
#' @param procs all verbs for SAS procedures (except DATA)
#'
#' @return list of identified procedures as
#' -proc : name of the procedure in listed values in procs
#' -contenu : what's inside the procedure
#'
#' @examples
#' get_SAS_procedure(text = "PROC CONTENTS DATA=sample;RUN;")
get_SAS_procedure <- function(text ,
                              procs = c(
                                "chart",
                                "contents",
                                "corr",
                                "freq",
                                "gchart",
                                "genmod",
                                "glm",
                                "gplot",
                                "means",
                                "plot",
                                "print",
                                "sql",
                                "summary",
                                "transpose",
                                "univariate"
                              )) {
  # Detect procedures
  procedures <- sapply(
    X = procs,
    FUN = function(x)
      if(x == "sql"){
        str_match(
          string  = text,
          pattern = "proc sql\\s?;\\s*(.*?)\\s*(run|quit);"
        )[, 2]
      }else{
        str_match(
          string  = text,
          pattern = paste0("proc ", x , "\\s*(.*?)\\s*(run|quit);")
        )[, 2]
      }

  )

  if (all(is.na(procedures))) {
    message("text not identified")
    return(NULL)
  }
  procedures <- procedures[!is.na(procedures)]

  # Verify length = 1 : only one procedure can be find if the split it done correctly
  if (length(procedures) > 1)
    warning("More than one procedure is found in the text")

  return(list(proc = names(procedures),
              contenu = unname(procedures)))

}
