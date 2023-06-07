data(models)

test_that("SEITL model classes", {
  ## test them
  theta <- c(
    "R_0" = 10, "D_lat" = 2, "D_inf" = 3, "alpha" = 0.5, "D_imm" = 15,
    "rho" = 0.7
  )
  initState <- c("S" = 280, "E" = 0, "I" = 2, "T" = 0, "L" = 4, "Inc" = 0)

  data("FluTdC1971", envir = environment())

  testFitmodel(
    fitmodel = SEITL_deter, theta = theta, initState = initState,
    data = FluTdC1971, verbose = TRUE
  )
  testFitmodel(
    fitmodel = SEITL_stoch, theta = theta, initState = initState,
    data = FluTdC1971, verbose = TRUE
  )

  initState <- c(
    "S" = 280, "E" = 0, "I" = 2, "T1" = 0, "T2" = 0, "L" = 4, "Inc" = 0
  )

  testFitmodel(
    fitmodel = SEIT2L_deter, theta = theta, initState = initState,
    data = FluTdC1971, verbose = TRUE
  )
  testFitmodel(
    fitmodel = SEIT2L_stoch, theta = theta, initState = initState,
    data = FluTdC1971, verbose = TRUE
  )

  expect_true(TRUE)
})
