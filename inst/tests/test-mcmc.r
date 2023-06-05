context("mcmc")

test_that("mcmcMH for deterministic SEITL model", {
  theta <- c(
    "R0" = 10, "D_lat" = 2, "D_inf" = 3, "alpha" = 0.5, "D_imm" = 15,
    "rho" = 0.7
  )
  initState <- c("S" = 280, "E" = 0, "I" = 2, "T" = 0, "L" = 4, "Inc" = 0)
  data("FluTdC1971", envir = environment())
  data <- FluTdC1971[1:5, ]

  target <- function(theta) {
    return(dLogPosterior(
      fitmodel = SEITL_deter, theta = theta, initState = initState, data = data,
      margLogLike = dTrajObs
    ))
  }

  # default covariance matrix
  suppressMessages(ans <- mcmcMH(
      target = target, initTheta = theta, nIterations = 100,
      adaptSizeStart = 10, adaptSizeCooling = 0.99, adaptShapeStart = 10,
      printInfoEvery = NULL
    ))

  expect_true(is.matrix(ans$trace))
  expect_true(is.numeric(ans$acceptanceRate))
  expect_true(is.matrix(ans$covmatEmpirical))
})

test_that("mcmcMH for deterministic SEIT2L model", {
  theta <- c(
    "R0" = 10, "D_lat" = 2, "D_inf" = 3, "alpha" = 0.5, "D_imm" = 15,
    "rho" = 0.7
  )
  initState <- c(
    "S" = 280, "E" = 0, "I" = 2, "T1" = 0, "T2" = 0, "L" = 4, "Inc" = 0
  )
  data("FluTdC1971", envir = environment())
  data <- FluTdC1971[1:5, ]

  target <- function(theta) {
    return(dLogPosterior(
      fitmodel = SEIT2L_deter, theta = theta, initState = initState,
      data = data, margLogLike = dTrajObs
    ))
  }

  # default covariance matrix
  suppressMessages(ans <- mcmcMH(
    target = target, initTheta = theta, nIterations = 100, adaptSizeStart = 10,
    adaptSizeCooling = 0.99, adaptShapeStart = 10, printInfoEvery = NULL
  ))

  expect_true(is.matrix(ans$trace))
  expect_true(is.numeric(ans$acceptanceRate))
  expect_true(is.matrix(ans$covmatEmpirical))
})


test_that("mcmcMH for stochastic SEITL model", {
  theta <- c(
    "R0" = 10, "D_lat" = 2, "D_inf" = 3, "alpha" = 0.5, "D_imm" = 15,
    "rho" = 0.7
  )
  initState <- c("S" = 280, "E" = 0, "I" = 2, "T" = 0, "L" = 4, "Inc" = 0)
  data("FluTdC1971", envir = environment())
  data <- FluTdC1971[1:5, ]

  target <- function(theta) {
    return(dLogPosterior(
      fitmodel = SEITL_stoch, theta = theta, initState = initState, data = data,
      margLogLike = margLogLikeSto, nParticles = 10
    ))
  }

  # default covariance matrix
  suppressMessages(ans <- mcmcMH(
    target = target, initTheta = theta, nIterations = 100, adaptSizeStart = 10,
    adaptSizeCooling = 0.99, adaptShapeStart = 10, printInfoEvery = NULL
  ))

  expect_true(is.matrix(ans$trace))
  expect_true(is.numeric(ans$acceptanceRate))
  expect_true(is.matrix(ans$covmatEmpirical))
})


test_that("mcmcMH for stochastic SEIT2L model", {
  theta <- c(
    "R0" = 10, "D_lat" = 2, "D_inf" = 3, "alpha" = 0.5, "D_imm" = 15,
    "rho" = 0.7
  )
  initState <- c(
    "S" = 280, "E" = 0, "I" = 2, "T1" = 0, "T2" = 0, "L" = 4, "Inc" = 0
  )
  data("FluTdC1971", envir = environment())
  data <- FluTdC1971[1:5, ]

  target <- function(theta) {
    return(dLogPosterior(
      fitmodel = SEIT2L_stoch, theta = theta, initState = initState,
      data = data, margLogLike = margLogLikeSto, nParticles = 10
    ))
  }

  # default covariance matrix
  suppressMessages(ans <- mcmcMH(
    target = target, initTheta = theta, nIterations = 100, adaptSizeStart = 10,
    adaptSizeCooling = 0.99, adaptShapeStart = 10, printInfoEvery = NULL
  ))

  expect_true(is.matrix(ans$trace))
  expect_true(is.numeric(ans$acceptanceRate))
  expect_true(is.matrix(ans$covmatEmpirical))
})
