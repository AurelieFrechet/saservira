test_that("Conversion libname simple", {
  code_sas = "libname librairie \"C:\\Users\\chemin\\dossier\""
  expect_equal(sasr_libname(code_sas),
  "librairie <- \"C:/Users/chemin/dossier\"")
})

#
# test_that("libname avec option", {
#
# })
