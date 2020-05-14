
# transform conditions ----------------------------------------------------

test_that("Equations", {
  chaine <-
    "var1 > 1 and var2 = \"frechet\" and var <> \"vide\""
  expect_equal(transform_conditions(chaine),
               "var1 > 1 & var2 == \"frechet\" & var != \"vide\"")
})


# transform functions -----------------------------------------------------

test_that("case when", {
  chaine <- "CASE
       WHEN a=1 THEN 'un'
       WHEN a=2 THEN 'deux'
       WHEN a=3 THEN 'trois'
       ELSE 'autre'
END"
  expect_equal(transform_functions(chaine),
               "case_when(a == 1 ~ 'un',\na == 2 ~ 'deux',\na == 3 ~ 'trois',\nTRUE ~ 'autre')")
})

test_that("fonctions simples", {
  chaine <- "avg(var1), count(*)"
  expect_equal(transform_functions(chaine), "mean(var1), n()")
})
