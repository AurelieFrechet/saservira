#  source : https://www.listendata.com/2015/01/sas-detailed-explanation-of-proc-means.html

test_that("Simple Example", {
  text = split_SAS_procedures("Proc Means Data = test;
  Var q1 - q5;
  Run;")
})

test_that("Limit Descriptive Statistics", {
  "Proc Means Data = test N NMISS;
  Var q1 - q5 ;
  Run;;"
})

test_that("Change Sorting Order", {
  "Proc Means Data = test;
  Class Age / descending;
  Var q1 - q5 ;
  Run;"
})

test_that("Group the analysis", {
  "Proc Means data = test N NMISS NOLABELS;
  Class Age;
  Var q1 - q5;
  Run;"
})


test_that("with output", {
  "Proc Means Data = test noprint;
Class Age ;
Var q1 q2;
Output out=F1 mean=  / autoname autolabel;
Run;"
})
