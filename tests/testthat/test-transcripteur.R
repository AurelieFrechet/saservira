test_that("test sur iris", {
  code_sas <- "proc contents data=iris; run;
  proc sql;
  select * from iris where Species=\"setosa\";
  quit;"
  expect_equal(traducteur(code_sas), "str(iris)\n  iris %>%\n\tfilter(species == \"setosa\")")

})
