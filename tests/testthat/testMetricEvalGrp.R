test_that("var_grp returns correct variable name", {
  var_grp <- VarGrp$new()

  soiln <- load_soiln_calc()

  var_grp$compare <- list(soiln = soiln)

  expect_equal(var_grp$var_name, "soiln")

  var_grp$compare <- NULL

  expect_error(var_grp$var_name, "No")
})
