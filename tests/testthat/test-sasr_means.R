
# With OUTPOUT ------------------------------------------------------------
test_that("proc means avec output iris", {
   code_sas = "proc means data = sashelp.iris;
              by Species;
              var PetalLength;
              output out = res mean=moyenne;
              run;"

expect_equal(
  sasr_means(code_sas),
  "res <- sashelp.iris %>%\n\tgroup_by(Species) %>%\n\tsummarize(moyenne = mean(PetalLength))")

})

test_that("proc means avec output test guido", {
  code_sas = "PROC MEANS DATA=fic1 ;
  VAR  x1 x2 x3 ind;
  OUTPUT OUT=fic2  mean=mx1 mx2 mx3  std= ex1 ex2 skewness=sx1 kurtosis=kx1;
  run;"

  expect_equal(
  sasr_means(code_sas),
  "fic2 <- fic1 %>%\n\tsummarize(mx1 = mean(x1), mx2 = mean(x2), mx3 = mean(x3), ex1 = sd(x1), ex2 = sd(x2), sx1 = skewness(x1), kx1 = kurtosis(x1))")
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



# Without indicators ---------------------------------------


test_that("proc means : multiple by and class and no indics", {
  code_sas = "proc means data = diamonds; var carat; by color; class cut; run;"
  expect_equal(
    sasr_means(code_sas),
    "diamonds %>%\n\tgroup_by(color, cut) %>%\n\tselect(carat) %>%\n\tsummary()"
  )
})

test_that("proc means : multiple variable and no indic", {
  code_sas = "proc means data=diamonds;
  var carat price;
  run;"
  expect_equal(
  sasr_means(code_sas),
  "diamonds %>%\n\tselect(carat, price) %>%\n\tsummary()"
  )
})

# Without var -------------------------------------------------------------

test_that("proc means without var", {
code_sas = "proc means data = sashelp.iris;
              run;"
  expect_equal(
    sasr_means(code_sas),
    "sashelp.iris %>%\n\tsummary()")
})

test_that("proc means without var with by", {
code_sas = "proc means data = sashelp.iris;
              by Species;
              run;"
expect_equal(sasr_means(code_sas),
             "sashelp.iris %>%\n\tgroup_by(Species) %>%\n\tsummary()")

})

