test_that("Import excel", {
  code_sas = "PROC import OUT= ca3_2016.pond_ca
  FILE = \"M:\\Usuels.DCT\\INDICATEURS\\TVA\\Divers\\prod.xls\"
  DBMS = XLS REPLACE
  SHEET = \"group\"
  getnames=yes;
  RUN ;"
})
