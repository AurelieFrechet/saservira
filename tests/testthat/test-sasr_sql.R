
# clause FROM ------------------------------------------------------------

test_that("sql_to_dplyr : clause from", {
  requete_sql <-
    "select *
     from table"
  expect_equal(sql_to_dplyr(requete_sql), "table")
})


# Clause SELECT -----------------------------------------------------------


test_that("sql_to_dplyr : selection simple", {
  requete_sql <-
    "var1, var2, var3"
  expect_equal(sql_dplyr_select(requete_sql),
               "select(\"var1\", \"var2\", \"var3\")")
})

test_that("sql_to_dplyr : creation variable", {
  requete_sql <-
    "old as new"
  expect_equal(sql_dplyr_select(requete_sql),
               "mutate(new = old) %>%\n\tselect(\"new\")")
})

test_that("sql_to_dplyr : calcul", {
  requete_sql <-
    "avg(age) as moy"
  expect_equal(sql_dplyr_select(requete_sql),
               "summarize(moy = mean(age))")
})



# Clause WHERE ------------------------------------------------------------


# Clause ORDER BY ---------------------------------------------------------


# Clause GROUP BY ---------------------------------------------------------


