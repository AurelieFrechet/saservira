

# sasr_sql  ---------------------------------------------------------------
test_that("sasr_sql : iris", {
  code_sas <-
    "proc sql;
    select * from iris where Species=\"setosa\";
  quit;"
  expect_equal(sasr_sql(code_sas), "iris %>%\n\tfilter(Species == \"setosa\")")

})


# clause FROM ------------------------------------------------------------

test_that("sql_to_dplyr : clause from", {
  requete_sql <-
    "select *
     from table"
  expect_equal(sql_to_dplyr(requete_sql), "table")
})


# Clause SELECT -----------------------------------------------------------


test_that("sql_dplyr_select : selection simple", {
  requete_sql <-
    "var1, var2, var3"
  expect_equal(sql_dplyr_select(requete_sql),
               "select(\"var1\", \"var2\", \"var3\")")
})

test_that("sql_dplyr_select : creation variable", {
  requete_sql <-
    "old as new"
  expect_equal(sql_dplyr_select(requete_sql),
               "mutate(new = old) %>%\n\tselect(\"new\")")
})

# test_that("sql_dplyr_select : calcul", {
#   requete_sql <-
#     "avg(age) as moy"
#   expect_equal(sql_dplyr_select(requete_sql),
#                "summarize(moy = mean(age))")
# })



# Clause WHERE ------------------------------------------------------------
test_that("sql_dplyr_where : équations", {
  requete_sql <-
    "var1 > 1 and var2 = \"frechet\" and var <> \"vide\""
  sql_dplyr_where(requete_sql)
  expect_equal(sql_dplyr_where(requete_sql),
               "filter(var1 > 1 & var2 == \"frechet\" & var != \"vide\")")
})

# Clause ORDER BY ---------------------------------------------------------


# Clause GROUP BY ---------------------------------------------------------


