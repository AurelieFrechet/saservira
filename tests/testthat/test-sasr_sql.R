

# sasr_sql  ---------------------------------------------------------------
test_that("sasr_sql : iris", {
  code_sas <-
    "proc sql;
    select * from iris where Species=\"setosa\";
  quit;"
  expect_equal(sasr_sql(code_sas), "iris %>%\n\tfilter(Species == \"setosa\")")

})

test_that("test boulot", {
  code_sas = "select rB010, rB030, RB080, RB0808F
  from table
  where (RB080 not between 1890 and 2018 and RB080 not = .)
  order by rb010, rB030;"
  sql_to_dplyr(code_sas)
})

test_that("test Rémi", {
  code_sas = "select cyl, sum(drat), max(drat)
  from mtcars
  group by cyl
  order by cyl"
  expect_equal(sql_to_dplyr(code_sas),
  "mtcars %>%\n\tgroup_by(cyl) %>%\n\tsummarize(sum(drat), max(drat)) %>%\n\tarrange(cyl)")
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
    "select var1, var2, var3
     from table"
  expect_equal(sql_to_dplyr(requete_sql),
               "table %>%\n\tselect(var1, var2, var3)")
})

test_that("sql_dplyr_select : creation variable 1", {
  requete_sql <-
    "select old as new
     from table"
  expect_equal(sql_to_dplyr(requete_sql),
               "table %>%\n\ttransmute(new = old)")
})

test_that("sql_dplyr_select : creation variable 2", {
  requete_sql <-
    "select *, old as new
     from table"
  expect_equal(sql_to_dplyr(requete_sql),
               "table %>%\n\tmutate(new = old)")
})

test_that("sql_dplyr_select : creation variable 3", {
  requete_sql <-
    "select var1, old as new
     from table"
  expect_equal(sql_to_dplyr(requete_sql),
               "table %>%\n\tmutate(new = old) %>%\n\tselect(var1, new)")
})

test_that("sql_dplyr_select : calcul ", {
  requete_sql <-
    "select avg(age)
     from table"
  expect_equal(sql_to_dplyr(requete_sql),
               "table %>%\n\tsummarize(mean(age))")
})

test_that("sql_dplyr_select : calcul avec nouvele var", {
  requete_sql <-
    "select avg(age) as moy
     from table"
  expect_equal(sql_to_dplyr(requete_sql),
               "table %>%\n\tsummarize(moy = mean(age))")
})


# Clause ORDER BY ---------------------------------------------------------

test_that("order by", {
  code_sql <- "select * from tbl1 order by var1, var2 desc"
  expect_equal(sql_to_dplyr(code_sql), "tbl1 %>%\n\tarrange(var1, -var2)")

})


# Clause GROUP BY ---------------------------------------------------------
test_that("group by", {
  code_sql <- "select var1, max(var2) as max from tbl1 group by var1"
  expect_equal(sql_to_dplyr(code_sql),
               "tbl1 %>%\n\tgroup_by(var1) %>%\n\tsummarize(max = max(var2))")

})

test_that("group by + having", {
  code_sql <- "select var1, count(*) as nb from tbl1 group by var1 having nb>1"
  expect_equal(sql_to_dplyr(code_sql),
               "tbl1 %>%\n\tgroup_by(var1) %>%\n\tsummarize(nb = n()) %>%\n\tfilter(nb>1)")

})



# Create Table ------------------------------------------------------------

test_that("create table as", {
  code_sql = "create table new_table as
  select * from old_table where var1 = 1"

})

