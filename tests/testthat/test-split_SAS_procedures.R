test_that("split diverse procedures", {
  text =     "proc contents data = table;
  run;
  proc means data= table;
  var age;
  run;
  proc freq data=table;
  table sexe;
  run;"
})


test_that("split similar procedures", {
  text =     "proc contents data = table;
  run;
  proc contents data= table2;
  run;
  proc contents data=table3;
  run;"
})

test_that("split everything", {
  text =
  "data test;
  set table;
  run;
  /* un commentaire ici*/
  proc contents data = table;
  run;
  proc means data= table;
  var age; /*et ici*/
  run;
  /*un commentaire là*/
  proc freq data=table;
  table sexe;
  run;"
})

test_that("complicated graph", {
  text =
    "data totals;
length dept $ 7 site $ 8;
input dept site quarter sales;
datalines;
Parts Sydney 1 7043.97
Parts Atlanta 1 8225.26
Parts Paris 1 5543.97
Tools Sydney 4 1775.74
Tools Atlanta 4 3424.19
Tools Paris 4 6914.25
;
  title 'Total Sales';
footnote j=r 'GCHBKSUM ';

proc gchart data=totals;
format sales dollar8.;
block site / sumvar=sales;
run;
quit;
"
})

