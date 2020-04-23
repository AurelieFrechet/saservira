test_that("decoupe sql", {
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
