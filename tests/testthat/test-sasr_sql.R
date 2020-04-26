
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
    "select id, age, sexe,
     from table"
  expect_equal(sql_to_dplyr(requete_sql),
               "table %>%\n\tselect(id,age,sexe)")
})

test_that("sql_to_dplyr : creation variable", {
  requete_sql <-
    "select old as new,
     from table"
  expect_equal(sql_to_dplyr(requete_sql),
               "table %>%\n\tmutate(new=old)%>%\n\tselect(new)")
})


test_that("sql_to_dplyr : calcul", {
  requete_sql <-
    "select avg(age) as moy,
     from table"
  expect_equal(sql_to_dplyr(requete_sql),
               "table %>%\n\tsummarize(moy=mean(age))")
})
