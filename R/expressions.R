#' Expressions regex
#'
#' @description différentes expression regex correspondants aux blocs principaux de code SAS
#'
#'@references http://www.lsta.upmc.fr/Lejeune/SAS_Lejeune.pdf
#'

"(?=^\\*).*?(?=\\n)" # Commentaire 1 ligne
"(?=^\\/\\*)(\\n|.)*?*?(?<=\\*\\/)" # Commentaire multilignes
"(?=^libname).*?(?<=;)" # libnanme


# Etape Data --------------------------------------------------------------
data_run       <- "(?=^data)(\\n|.)*?(?<=run;)"

# Procédures --------------------------------------------------------------
# PROC CONTENTS
proc_contents  <- "(?=^proc contents)(.|\\n)*?(?<=run;)"
# PROC PRINT
proc_print     <- "(?=^proc print)(.|\\n)*?(?<=run;)"
# PROC SORT
proc_sort      <- "(?=^proc sort)(.|\\n)*?(?<=run;)"
# PROC STANDARD
proc_standard  <- "(?=^proc standard)(.|\\n)*?(?<=run;)"
# PROC TRANSPOSE
proc_transpose <- "(?=^proc transpose)(.|\\n)*?(?<=run;)"
# PROC MEANS
proc_means     <- "(?=^proc means)(.|\\n)*?(?<=run;)"
# PROC CORR
proc_corr      <- "(?=^proc corr)(.|\\n)*?(?<=run;)"
# PROC UNIVARIATE
proc_univariate<- "(?=^proc univariate)(.|\\n)*?(?<=run;)"
# PROC FREQ
proc_freq      <- "(?=^proc freq)(.|\\n)*?(?<=run;)"
# PROC REG
proc_reg       <- "(?=^proc reg)(.|\\n)*?(?<=run;)"
# PROC GLM
proc_glm       <- "(?=^proc glm)(.|\\n)*?(?<=run;)"
# PROC GENMOD
proc_genmod    <- "(?=^proc genmod)(.|\\n)*?(?<=run;)"
# PROC PLOT
proc_plot      <- "(?=^proc plot)(.|\\n)*?(?<=run;)"
# PROC GPLOT
proc_gplot     <- "(?=^proc gplot)(.|\\n)*?(?<=run;)"
# PROC PLOT
proc_chart     <- "(?=^proc chart)(.|\\n)*?(?<=run;)"
# PROC GPLOT
proc_gchart    <- "(?=^proc gchart)(.|\\n)*?(?<=run;)"



