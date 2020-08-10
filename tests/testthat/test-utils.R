
# data_equal_to -----------------------------------------------------------

test_that("data equal to sans librairie", {
code_sas = "proc contents data=table_1;run;"
expect_equal(data_equal_to(code_sas),
             "table_1")

})

test_that("data equal to avec librairie", {
  code_sas = "proc contents data=lib4.table_1;run;"
  expect_equal(
    data_equal_to(code_sas),
    "file.path(lib4, \"table_1\")"
  )

})


# transform conditions ----------------------------------------------------

test_that("Equations", {
  chaine <-
    "var1 > 1 and var2 = \"frechet\" and var <> \"vide\""
  expect_equal(transform_conditions(chaine),
               "var1 > 1 & var2 == \"frechet\" & var != \"vide\"")
})

test_that("conditions", {
  expect_equal(transform_conditions("utilisateur_id IS NOT NULL"), "!is.na(utilisateur_id)")
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


# transform list ----------------------------------------------------------

test_that("liste character", {
  chaine <- "{l1 l2 l3 l4 l5 }"
  expect_equal(
    transform_list(chaine),
    "c(\"l1\", \"l2\", \"l3\", \"l4\", \"l5\")"
  )

})

test_that("liste numerique", {
  chaine <- "{1 2 3 4 5 }"
  expect_equal(
    transform_list(chaine),
    "c(1, 2, 3, 4, 5)"
  )

})



# transform path ----------------------------------------------------------

test_that("test de chemin SAS", {
  chaine <- "M:\\Chemin\\Dossier\\Sous-dossier\\fichier.xls"
  expect_equal(transform_path(chaine),
  "M:/Chemin/Dossier/Sous-dossier/fichier.xls")
})
