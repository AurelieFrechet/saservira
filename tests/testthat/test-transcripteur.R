test_that("test sur iris", {
  code_sas <- "proc contents data=iris; run;
  proc sql;
  select * from iris where Species=\"setosa\";
  quit;"
  expect_equal(traducteur(code_sas), "str(iris)\n  iris %>%\n\tfilter(Species == \"setosa\")")

})


test_that("vide", {
  code_sas <- ""
  expect_equal(traducteur(code_sas), "")

})

test_that("code non reconnu", {
  code_sas <- "code non reconnu"
  expect_equal(traducteur(code_sas), "code non reconnu")

})


test_that("sasr_sql : code de William", {
  code_sas <- "Proc sql noprint;
  select date as date format yyddmm8., field1, length(field2), catx('|',field3,field4) as concat
  from my_lib.my_table
  order by date;
quit;"
  traducteur(code_sas)
})
