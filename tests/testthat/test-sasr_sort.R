test_that("Sort simple", {
  code_sas = "proc sort data=agregtva;
  by AA_CODET;
  run;"

  expect_equal(sasr_sort(code_sas),
  "agregtva %>% arrange(AA_CODET)")
})


test_that("Sort descendant et out : exemple support sas", {
  code_sas = "proc sort data=account out =sorted;
  by town descending debt accountnumber;
  run;"

  expect_equal(sasr_sort(code_sas),
  "sorted <- account %>% arrange(town, desc(debt), accountnumber)")
})
