data(models)

test_that("simulate and generate observation", {
  # SEITL
  listModel <- list(seitlDeter, seitlStoch)

  theta <- c(
    "R_0" = 10, "D_lat" = 2, "D_inf" = 3, "alpha" = 0.5, "D_imm" = 15,
    "rho" = 0.7
  )
  initState <- c("S" = 280, "E" = 0, "I" = 2, "T" = 0, "L" = 4, "Inc" = 0)
  times <- 0:58

  for (model in listModel) {
    traj <- model$simulate(theta = theta, initState, times = times)
    expect_true(inherits(traj, "data.frame"))

    trajObs <- rTrajObs(model, theta, initState, times)
    expect_true(inherits(trajObs, "data.frame"))
  }

  # SEITL2
  listModel <- list(seit2lDeter, seit2lStoch)

  initState <- c(
    "S" = 280, "E" = 0, "I" = 2, "T1" = 0, "T2" = 0, "L" = 4, "Inc" = 0
  )

  for (model in listModel) {
    traj <- model$simulate(theta = theta, initState, times = times)
    expect_true(inherits(traj, "data.frame"))

    trajObs <- rTrajObs(model, theta, initState, times)
    expect_true(inherits(trajObs, "data.frame"))
  }
})
