test_that("test sur iris", {
  code_sas <- "proc contents data=iris; run;
  proc sql;
  select * from iris where Species=\"setosa\";
  quit;"
  expect_equal(traducteur(code_sas), "str(iris)\n  iris %>%\n\tfilter(species == \"setosa\")")

})


test_that("vide", {
  code_sas <- ""
  expect_equal(traducteur(code_sas), "")

})

test_that("code non reconnu", {
  code_sas <- "code non reconnu"
  expect_equal(traducteur(code_sas), "code non reconnu")

})
