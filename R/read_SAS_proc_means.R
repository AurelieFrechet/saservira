#' read PROC MEANS
#' @description
#' source : https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/proc/p0f0fjpjeuco4gn1ri963f683mi4.htm
#' PROC MEANS <options> <statistic-keyword(s)>;
#' BY <DESCENDING> variable-1 <<DESCENDING> variable-2 ...> <NOTSORTED>;
#' CLASS variable(s) </ options>;
#' FREQ variable;
#' ID variable(s);
#' OUTPUT <OUT=SAS-data-set> <output-statistic-specification(s)>
#'   <id-group-specification(s)> <maximum-id-specification(s)>
#'   <minimum-id-specification(s)> </ options> ;
#' TYPES request(s);
#' VAR variable(s) </ WEIGHT=weight-variable>;
#' WAYS list;
#' WEIGHT variable;
#' @import stringr
#' @param text
#'
#' @return
#' @export
#'
#' @examples
read_SAS_proc_means <- function(text) {

match_data   <- str_match_all(text, "data\\s?=\\s?(\\w+)((\\s\\w+)+)?") %>%  unlist()
match_by     <- str_match_all(text, "by(\\s(\\w+)?\\w+)+") %>%  unlist()
match_class  <- str_match_all(text, "class(\\s\\w+)+") %>%  unlist()
match_freq   <- str_match_all(text, "freq(\\s\\w+)+") %>%  unlist()
match_id     <- str_match_all(text, "id(\\s\\w+)+") %>%  unlist()
match_output <- str_match_all(text, "output out\\s?=\\s?(\\w+)") %>%  unlist()
match_types  <- str_match_all(text, "types(\\s\\w+)+") %>%  unlist()
match_var    <- str_match_all(text, "var(\\s\\w+)+") %>%  unlist()
match_ways   <- str_match_all(text, "ways(\\s\\w+)+") %>%  unlist()
match_weight <- str_match_all(text, "weight(\\s\\w+)+") %>%  unlist()

}
