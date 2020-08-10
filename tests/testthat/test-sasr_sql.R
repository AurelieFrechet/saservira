
# utils SQL -------------------------------------------------------------
test_that("get_alias", {
  expect_equal(get_alias("table2 as t2"), "t2")
  expect_equal(get_alias("table2 t2"), "t2")
  expect_equal(get_alias("table2"), "table2")
})

test_that("get_table", {
  expect_equal(get_table("table2 as t2"), "table2")
  expect_equal(get_table("table2 t2"), "table2")
  expect_equal(get_table("table2"), "table2")
})

test_that("read_join", {
  expect_equal(read_join("t1", "t2", "t1.id = t2.fk_id"), "\"id\" = \"fk_id\"")
  expect_equal(read_join("t1", "t2", "t2.fk_id = t1.id"), "\"id\" = \"fk_id\"")
})


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
  code_sql <-
    "select *
     from table"
  expect_equal(sql_to_dplyr(code_sql), "table")
})


# Clause SELECT -----------------------------------------------------------


test_that("sql_dplyr_select : selection simple", {
  code_sql <-
    "select var1, var2, var3
     from table"
  expect_equal(sql_to_dplyr(code_sql),
               "table %>%\n\tselect(var1, var2, var3)")
})

test_that("sql_dplyr_select : creation variable 1", {
  code_sql <-
    "select old as new
     from table"
  expect_equal(sql_to_dplyr(code_sql),
               "table %>%\n\ttransmute(new = old)")
})

test_that("sql_dplyr_select : creation variable 2", {
  code_sql <-
    "select *, old as new
     from table"
  expect_equal(sql_to_dplyr(code_sql),
               "table %>%\n\tmutate(new = old)")
})

test_that("sql_dplyr_select : creation variable 3", {
  code_sql <-
    "select var1, old as new
     from table"
  expect_equal(sql_to_dplyr(code_sql),
               "table %>%\n\tmutate(new = old) %>%\n\tselect(var1, new)")
})

test_that("sql_dplyr_select : calcul ", {
  code_sql <-
    "select avg(age)
     from table"
  expect_equal(sql_to_dplyr(code_sql),
               "table %>%\n\tsummarize(mean(age))")
})

test_that("sql_dplyr_select : calcul avec nouvele var", {
  code_sql <-
    "select avg(age) as moy
     from table"
  expect_equal(sql_to_dplyr(code_sql),
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

  expect_equal(sql_to_dplyr(code_sql),
  "new_table <- old_table %>%\n\tfilter(var1 == 1)")

})

test_that("requete Sylvain 1", {
  code_sql = "
  CREATE TABLE LIB.MY_IRIS AS
  SELECT *, SepalLength*SepalWidth as result
  FROM SASHELP.IRIS
  "
  expect_equal(sql_to_dplyr(code_sql),
  "LIB.MY_IRIS <- SASHELP.IRIS %>%\n\tmutate(result = SepalLength*SepalWidth)")

})




# Jointures ---------------------------------------------------------------
#source : https://sql.sh/cours/jointures/inner-join
# https://www.w3schools.com/sql/sql_join_left.asp

test_that("Jointure simple avec ON", {
  code_sql = "SELECT *
FROM table1
INNER JOIN table2 ON table1.id = table2.fk_id"

  expect_equal(
    sql_to_dplyr(code_sql),
    "table1 %>%\n\tinner_join(table2, by = c(\"id\" = \"fk_id\"))"
  )
})

test_that("Double jointure simple avec ON", {
  code_sql = "SELECT *
FROM table1
INNER JOIN table2 ON table1.id = table2.fk_id
INNER JOIN table3 ON table3.nom = table1.nom"

  expect_equal(
    sql_to_dplyr(code_sql),
    "table1 %>%\n\tinner_join(table2, by = c(\"id\" = \"fk_id\")) %>%\n\tinner_join(table3, by = c(\"nom\" = \"nom\"))"
  )
})

test_that("Jointure simple avec WHERE", {
  code_sql = "SELECT *
FROM table1
INNER JOIN table2
WHERE table1.id = table2.fk_id"

  expect_equal(
    sql_to_dplyr(code_sql),
    "table1 %>%\n\tinner_join(table2, by = c(\"id\" = \"fk_id\"))"
  )
})

test_that("Jointure simple avec ON et selection de variables", {
  code_sql = "SELECT id, prenom, nom, date_achat, num_facture, prix_total
FROM utilisateur
INNER JOIN commande ON utilisateur.id = commande.utilisateur_id"

  expect_equal(
    sql_to_dplyr(code_sql),
    "utilisateur %>%\n\tinner_join(commande, by = c(\"id\" = \"utilisateur_id\")) %>%\n\tselect(id, prenom, nom, date_achat, num_facture, prix_total)"
  )
})

test_that("Jointure simple avec ON, selection de variables et filtre", {
  code_sql = "SELECT id, prenom, nom, utilisateur_id
FROM utilisateur
LEFT JOIN commande ON utilisateur.id = commande.utilisateur_id
WHERE utilisateur_id IS NOT NULL"
  expect_equal(sql_to_dplyr(code_sql),
               "utilisateur %>%\n\tleft_join(commande, by = c(\"id\" = \"utilisateur_id\")) %>%\n\tselect(id, prenom, nom, utilisateur_id) %>%\n\tfilter(!is.na(utilisateur_id))")
})

test_that("Jointure multiple", {
  code_sql = "SELECT Orders.OrderID, Customers.CustomerName, Shippers.ShipperName
FROM ((Orders
INNER JOIN Customers ON Orders.CustomerID = Customers.CustomerID)
INNER JOIN Shippers ON Orders.ShipperID = Shippers.ShipperID); "
})

test_that("Jointure impropre", {
  code_sql = "SELECT Orders.OrderID, Orders.OrderDate, Customers.CustomerName
FROM Customers
INNER JOIN Orders ON Customers.CustomerID=Orders.CustomerID
WHERE Customers.CustomerName='Around the Horn'"
  sql_to_dplyr(code_sql)

"SELECT Orders.OrderID, Orders.OrderDate, Customers.CustomerName
FROM Customers, Orders
WHERE Customers.CustomerName='Around the Horn' AND Customers.CustomerID=Orders.CustomerID;"

  })

test_that("requete Sylvain 2", {
  code_sql = "CREATE TABLE LIB.JOINTURE AS
  SELECT DISTINCT a.CUSTUMER_ID, a.DATE, sum(b.price) as total_price
  FROM LIB2.TABLE1 as a
  LEFT JOIN LIB3.TABLE2 b
  ON a.CUSTUMER_ID = b.CUSTUMER_ID and a.var1 != b.var2
  WHERE a.DATE < mdy(1, 1, 2020)
  GROUP BY a.CUSTUMER_ID, a.DATE
  HAVING calculated total_price>0"

  "LIB2.TABLE1 %>%
  left_join(LIB3.TABLE2, by = c(\"CUSTUMER_ID\"=\"CUSTUMER_ID\")) %>%
  filter(var1 != var2 & DATE < mdy(1, 1, 2020)) %>%
  group_by(CUSTUMER_ID, DATE) %>%
  summarize(total_price = sum(price)) %>%
  filter(total_price > 0)"
})


