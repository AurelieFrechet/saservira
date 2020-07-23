test_that("Summary avec outpout : source stackoverflow", {
  code_sas = "PROC SUMMARY data = test;
  class flag1 flag2 flag3;
  var Type1;
  output=final_data Sum=sum(Type1);
  run;"

  "final_data <- test %>%
  group_by(flag1, flag2, flag3) %>%
  summary(Sum=sum(Type1)) %>%
  ungroup()"
})
