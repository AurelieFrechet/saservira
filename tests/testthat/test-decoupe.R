
# decoupe_requete --------------------------------------------------------

test_that("decoupe sql - Cas normal", {
  code_sql <- "select * from table where nom=\"frechet\""
  sentence <- decoupe_requete(code_sql,
                              key_words = c("select",
                                            "from",
                                            "where",
                                            "order by",
                                            "group by",
                                            "limit"))
  expect_length(sentence, 3)
  expect_equal(names(sentence),
               c("select", "from", "where"))
  compare(sentence,
               c("*", "table", "nom=\"frechet\""),
          check.attributes = FALSE,
          )

})

test_that("decoupe sql - Ignore CASE", {
  code_sql <- "SELECT * FROM table WHERE nom=\"Frechet\""
  sentence <- decoupe_requete(code_sql,
                              key_words = c("select",
                                            "from",
                                            "where",
                                            "order by",
                                            "group by",
                                            "limit"))
  expect_length(sentence, 3)
  expect_equal(names(sentence),
               c("select", "from", "where"))
  compare(sentence,
          c("*", "table", "nom=\"frechet\""),
          check.attributes = FALSE,
  )

})


test_that("decoupe sql - Cas vide", {
code_sql <- "phrase qui n'a aucun rapport"
sentence <- decoupe_requete(code_sql,
                            key_words = c("select",
                                          "from",
                                          "where",
                                          "order by",
                                          "group by",
                                          "limit"))
expect_null(sentence)
expect_message(decoupe_requete(code_sql,
                               key_words = c("select",
                                             "from",
                                             "where",
                                             "order by",
                                             "group by",
                                             "limit")),
               "Requete does not contain key words")

})


# decouper_SAS ------------------------------------------------------------



test_that("decoupe sas - Code non reconnu",{
  code_sas     <- "Ceci n'est pas du code SAS"
  code_decoupe <- decouper_SAS(code_sas)
  expect_length(code_decoupe, 3) # liste de taille 3
  expect_equal(names(code_decoupe), c("place", "texte", "id"))
  expect_length(code_decoupe$place, 0)
  expect_length(code_decoupe$texte, 0)
  expect_length(code_decoupe$id, 0)
})


test_that("decoupe sas - Commentaires",{
  code_sas     <-
  "/*Ceci est un commentaire*/
  /*Ceci
  est un
  commentaire
  multilignes*/
  *Ceci est une ligne
  Ceci n'est pas un commentaire * "
  code_decoupe <- decouper_SAS(code_sas)
  expect_length(code_decoupe, 3) # liste de taille 3
  expect_equal(names(code_decoupe), c("place", "texte", "id"))
  expect_equal(code_decoupe$texte, c("  *Ceci est une ligne",
                                     "/*Ceci est un commentaire*/",
                                     "/*Ceci\n  est un\n  commentaire\n  multilignes*/"))
  expect_equal(code_decoupe$id, c("*", "/*", "/*"))
})


test_that("decoupe sas - Procédures",{
  code_sas     <-
    "proc contents data = table;
  run;
  proc means data= table;
  var age;
  run;
  proc freq data=table;
  table sexe;
  run;"
  code_decoupe <- decouper_SAS(code_sas)
  expect_length(code_decoupe, 3) # liste de taille 3
  expect_equal(names(code_decoupe), c("place", "texte", "id"))
  expect_equal(code_decoupe$texte, c("proc contents data = table;\n  run;",
                                     "proc means data= table;\n  var age;\n  run;",
                                     "proc freq data=table;\n  table sexe;\n  run;"))
  expect_equal(code_decoupe$id, c("proc contents",
                                  "proc means",
                                  "proc freq" ))
})

