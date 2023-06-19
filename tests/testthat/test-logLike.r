data(models)

test_that("log-like deter of SEITL", {
  theta <- c(
    R_0 = 10, D_lat = 2, D_inf = 3, alpha = 0.5, D_imm = 15, rho = 0.7
  )
  initState <- c(S = 280, E = 0, I = 2, T = 0, L = 4, Inc = 0)
  data(fluTdc1971, envir = environment())
  data <- fluTdc1971[1:5, ]

  x <- dTrajObs(
    fitmodel = seitlDeter, theta = theta, initState = initState, data = data
  )
  expect_true(is.numeric(x))
})


test_that("log-like deter of SEIT2L", {
  theta <- c(
    R_0 = 10, D_lat = 2, D_inf = 3, alpha = 0.5, D_imm = 15, rho = 0.7
  )
  initState <- c(
    S = 280, E = 0, I = 2, T1 = 0, T2 = 0, L = 4, Inc = 0
  )
  data(fluTdc1971, envir = environment())
  data <- fluTdc1971[1:5, ]

  x <- dTrajObs(
    fitmodel = seit2lDeter, theta = theta, initState = initState, data = data
  )
  expect_true(is.numeric(x))
})

test_that("log-like sto of SEITL", {
  theta <- c(
    R_0 = 10, D_lat = 2, D_inf = 3, alpha = 0.5, D_imm = 15, rho = 0.7
  )
  initState <- c(S = 280, E = 0, I = 2, T = 0, L = 4, Inc = 0)
  data(fluTdc1971, envir = environment())
  data <- fluTdc1971[1:5, ]

  previousPlan <- future::plan()
  future::plan("multisession")
  x <- margLogLikeSto(
    fitmodel = seitlStoch, theta = theta, initState = initState, data = data,
    nParticles = 10
  )
  future::plan(previousPlan)
  expect_true(is.numeric(x))
})


test_that("log-like sto of SEIT2L", {
  theta <- c(
    R_0 = 10, D_lat = 2, D_inf = 3, alpha = 0.5, D_imm = 15, rho = 0.7
  )
  initState <- c(
    S = 280, E = 0, I = 2, T1 = 0, T2 = 0, L = 4, Inc = 0
  )
  data(fluTdc1971, envir = environment())
  data <- fluTdc1971[1:5, ]

  previousPlan <- future::plan()
  future::plan("multisession")
  x <- margLogLikeSto(
    fitmodel = seit2lStoch, theta = theta, initState = initState, data = data,
    nParticles = 10
  )
  future::plan(previousPlan)
  expect_true(is.numeric(x))
})

test_that("posterior deter of SEITL", {
  theta <- c(
    R_0 = 10, D_lat = 2, D_inf = 3, alpha = 0.5, D_imm = 15, rho = 0.7
  )
  initState <- c(S = 280, E = 0, I = 2, T = 0, L = 4, Inc = 0)
  data(fluTdc1971, envir = environment())
  data <- fluTdc1971[1:5, ]

  x <- dLogPosterior(
    fitmodel = seitlDeter, theta = theta, initState = initState, data = data,
    margLogLike = dTrajObs
  )
  expect_true(is.numeric(x$logDensity))
})


test_that("posterior deter of SEIT2L", {
  theta <- c(
    R_0 = 10, D_lat = 2, D_inf = 3, alpha = 0.5, D_imm = 15, rho = 0.7
  )
  initState <- c(
    S = 280, E = 0, I = 2, T1 = 0, T2 = 0, L = 4, Inc = 0
  )
  data(fluTdc1971, envir = environment())
  data <- fluTdc1971[1:5, ]

  x <- dLogPosterior(
    fitmodel = seit2lDeter, theta = theta, initState = initState, data = data,
    margLogLike = dTrajObs
  )
  expect_true(is.numeric(x$logDensity))
})


test_that("posterior sto of SEITL", {
  theta <- c(
    R_0 = 10, D_lat = 2, D_inf = 3, alpha = 0.5, D_imm = 15, rho = 0.7
  )
  initState <- c(S = 280, E = 0, I = 2, T = 0, L = 4, Inc = 0)
  data(fluTdc1971, envir = environment())
  data <- fluTdc1971[1:5, ]

  previousPlan <- future::plan()
  future::plan("multisession")
  x <- dLogPosterior(
    fitmodel = seitlStoch, theta = theta, initState = initState, data = data,
    margLogLike = margLogLikeSto, nParticles = 10
  )
  future::plan(previousPlan)
  expect_true(is.numeric(x$logDensity))
})


test_that("posterior sto of SEIT2L", {
  theta <- c(
    R_0 = 10, D_lat = 2, D_inf = 3, alpha = 0.5, D_imm = 15, rho = 0.7
  )
  initState <- c(
    S = 280, E = 0, I = 2, T1 = 0, T2 = 0, L = 4, Inc = 0
  )
  data(fluTdc1971, envir = environment())
  data <- fluTdc1971[1:5, ]

  previousPlan <- future::plan()
  future::plan("multisession")
  x <- dLogPosterior(
    fitmodel = seit2lStoch, theta = theta, initState = initState, data = data,
    margLogLike = margLogLikeSto, nParticles = 10
  )
  future::plan(previousPlan)
  expect_true(is.numeric(x$logDensity))
})
