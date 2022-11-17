# not a procedure ----
test_that("not a procedure", {
  expect_null(get_SAS_procedure("title = 'Un super titre'"))
})


# all procedures ----
test_that("PROC CHART", {
  expect_equal(
    get_SAS_procedure(
      "proc chart data=shirts; vbar size; title 'Number of Each Shirt Size Sold'; run;"
    ),
    list(proc = "chart",
         contenu = "data=shirts; vbar size; title 'Number of Each Shirt Size Sold';")
  )
})

test_that("PROC CONTENTS", {
  expect_equal(
    get_SAS_procedure("proc contents data=sample; run;"),
    list(proc = "contents",
         contenu = "data=sample;")
  )

})

test_that("PROC CORR", {
  ""
})

test_that("PROC FREQ", {
  expect_equal(
    get_SAS_procedure(text = "proc freq data = example1; tables y; run;"),
    list(proc = "freq",
         contenu = "data = example1; tables y;")
  )
})

test_that("PROC GCHART", {
  expect_equal(
    get_SAS_procedure(
      "proc gchart data=totals; format sales dollar8.; block site / sumvar=sales; run; quit;"
    ),
    list(proc = "gchart",
         contenu = "data=totals; format sales dollar8.; block site / sumvar=sales;")
  )
})

test_that("PROC GENMOD", {
  ""
})

test_that("PROC GLM", {
  ""
})

test_that("PROC GPLOT", {
  ""
})

test_that("PROC MEANS", {
  expect_equal(
    get_SAS_procedure(text = "proc means data=sashelp.cars q1 median q3; var msrp invoice; run;"),
  list(proc = "means",
       contenu = "data=sashelp.cars q1 median q3; var msrp invoice;")
  )
})

test_that("PROC PLOT", {
  expect_equal(
    get_SAS_procedure(
  "proc plot data=djia; plot high*year='*' / vspace=5 vaxis=by 1000; title 'High Values of the Dow Jones Industrial Average'; title2 'from 1968 to 2008'; run;"),
  list(proc = "plot",
       contenu = "data=djia; plot high*year='*' / vspace=5 vaxis=by 1000; title 'High Values of the Dow Jones Industrial Average'; title2 'from 1968 to 2008';")
  )
  })

test_that("PROC PRINT", {
  ""
})

test_that("PROC SQL", {
  expect_equal(
    get_SAS_procedure(
  "proc sql; select * from sashelp.cars where Type='Sports' and MSRP>100000; run;"),
  list(proc = "sql",
       contenu = "select * from sashelp.cars where Type='Sports' and MSRP>100000;")
  )
})

test_that("PROC SUMMARY", {
  ""
})

test_that("PROC TRANSPOSE", {
  ""
})

test_that("PROC UNIVARIATE", {
  ""
})
