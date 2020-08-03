
# With OUTPOUT ------------------------------------------------------------
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

  code_sas = "proc means data = sashelp.iris;
              by Species;
              var PetalLength;
              output out = res mean=moyenne;
              run;"

  "res <- iris %>%
  group_by(Species) %>%
  summarize(moyenne = mean(PetalLength))"

  "PROC MEANS DATA=fic1 ;
  VAR  x1 x2 x3 ind;
  OUTPUT OUT=fic2  mean=mx1 mx2 mx3  std= ex1e x2 skewness=sx1 kurtosis=kx1;
  run;"

})

# Without OUTPUT ----------------------------------------------------------

test_that("proc means : mean and sum on multiple variable", {
  code_sas = "proc means data=iris mean sum; var Sepal.Length Sepal.Width Petal.Length Petal.Width; run;"

  expect_equal(
    sasr_means(code_sas),
    "iris %>%\n\tselect(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%\n\tsummarize_all(list(mean=mean, sum=sum))"
  )
})

test_that("proc means : variables separated by -", {
  code_sas = "proc means data=iris mean sum; var Sepal.Length-Petal.Width; run;"

  expect_equal(
    sasr_means(code_sas),
    "iris %>%\n\tselect(Sepal.Length:Petal.Width) %>%\n\tsummarize_all(list(mean=mean, sum=sum))"
  )
})

test_that("proc means : one var", {
  code_sas = "proc means data=iris mean sum; var Sepal.Length; run;"

  expect_equal(
    sasr_means(code_sas),
    "iris %>%\n\tsummarize(mean(Sepal.Length), sum(Sepal.Length))"
  )
})

test_that("proc means : one var, correct indic", {
  code_sas = "proc means data=iris n mean sum p25 p1; var Sepal.Length; run;"

  expect_equal(
    sasr_means(code_sas),
    "iris %>%\n\tsummarize(n(), mean(Sepal.Length), sum(Sepal.Length), quantile(Sepal.Length, 25/100), quantile(Sepal.Length, 1/100))"
  )
})

test_that("proc means : multiple by and class and no indics", {
  code_sas = "proc means data = diamonds; var carat; by color; class cut; run;"
  expect_equal(
  sasr_means(code_sas),
  "diamonds %>%\n\tgroup_by(color, cut) %>%\n\tsummarize(n(), mean(carat), sd(carat), min(carat), max(carat))"
  )
})

# Simple request without indicators ---------------------------------------

test_that("iris : proc means simple", {
  code_sas = "proc means data = sashelp.iris;
              by Species;
              run;"

  "by(iris, iris$Species, summary)"


  code_sas = "proc means data = sashelp.iris;
              run;"

  "summary(iris)"

})








