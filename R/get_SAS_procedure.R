#' get SAS procedures
#' @import stringr
#' @import magrittr
#' @description read SAS code, identify procedures and extract their contents
#' @param text SAS code
#' @return list of identified procedures
#' @export
#'
#' @examples
#' get_SAS_procedure(text = "PROC CONTENTS DATA=sample;RUN;")
get_SAS_procedure <- function(text) {
  proc <- c("chart", "contents", "corr",
            "freq", "gchart", "genmod", "glm", "gplot",
            "means", "plot", "print",
            "sql", "summary", "transpose", "univariate")

  # Clean Text
  text <- text %>%
    tolower() %>%
    str_squish() %>%
    str_replace_all(pattern = regex("run\\s?;",
                                    ignore_case = TRUE),
                    replacement = "run;") %>%
    str_replace_all(pattern = regex("quit\\s?;",
                                    ignore_case = TRUE),
                    replacement = "quit;")


  # Detect procedures
  procedures <- lapply(
    X = proc,
    FUN = function(x)
      str_match_all(string  = text,
                    pattern = paste0("proc ",x ,"\\s*(.*?)\\s*run;"))[[1]][, 2]
  )

  names(procedures) <- paste("proc", proc, sep = "_")

}
