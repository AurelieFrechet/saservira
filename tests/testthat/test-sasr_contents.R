test_that("proc contents", {
  code_sas <- "proc contents data=iris; run;"
  expect_equal(sasr_contents(code_sas), "str(iris)")
})
