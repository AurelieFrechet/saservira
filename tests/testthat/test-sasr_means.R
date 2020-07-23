test_that("proc means avec output", {
  code_sas = "proc means data=b2 noprint;
  by AA_PRF;
  var AA_IND;
  weight pond;
  output out =res(drop=_type_ _freq_)
  mean=;
  run;"

  "res <- b2 %>%
  group_by(AA_PRF) %>%
  summarize(mean=weighted.mean(AA_IND, pond))"


})

test_that("iris : proc means simple", {
  code_sas = "proc means data = sashelp.iris;
              by Species;
              run;"

  "by(iris, iris$Species, summary)"


  code_sas = "proc means data = sashelp.iris;
              run;"

  "summary(iris)"

  code_sas = "proc means data = sashelp.iris;
              by Species;
              var PetalLength;
              output out = res mean=moyenne;
              run;"

  "res <- iris %>%
  group_by(Species) %>%
  summarize(moyenne = mean(PetalLength))"
})
