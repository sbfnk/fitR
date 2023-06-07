#' Run a particle filter for fitmodel object
#'
#' The particle filter returns an estimate of the marginal log-likelihood \eqn{L
#' = p(y(t_{1:T})|\theta)} as well as the set of filtered trajectories and their
#' respective weights at the last observation time
#' \eqn{\omega(t_T)=p(y(t_T)|\theta)}.
#' @param nParticles number of particles
#' @param progress if \code{TRUE} progression of the filter is displayed in the
#'   console.
#' @param nCores number of cores on which propogation of the particles is
#'   parallelised. By default no parallelisation (\code{nCores=1}). If
#'   \code{NULL}, set to the value returned by
#'   \code{\link[parallel]{detectCores}}.
#' @inheritParams testFitmodel
#' @note An unbiased state sample \eqn{x(t_{0:T}) ~
#'   p(X(t_{0:T})|\theta,y(t_{0:T}))} can be obtained by sampling the set of
#'   trajectories \code{traj} with probability \code{trajWeight}.
#' @export
#' @seealso plotSMC
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom furrr future_map furrr_options
#' @return A list of 3 elements:
#' \itemize{
#' \item \code{dPointObs} the marginal log-likelihood of the theta.
#' \item \code{traj} a list of size \code{nParticles} with all filtered
#'   trajectories.
#' \item \code{trajWeight} a vector of size \code{nParticles} with the
#'   normalised weight of the filtered trajectories.
#' }
particleFilter <- function(fitmodel, theta, initState, data, nParticles,
                           progress = FALSE, nCores = 1) {
  ## compute the margLogLike using a particle filter

  # initialisation

  # marginal log-likelihood of theta
  margLogLike <- 0

  # initial state of particles
  currentStateParticles <- rep(list(initState), nParticles)

  # filtered trajectories (just add time variable to initial state)
  trajParticles <- rep(
    list(data.frame(t(c(time = 0, initState)))), nParticles
  )

  # weight of particles
  weightParticles <- rep(1 / nParticles, length = nParticles)

  if (progress) {
    # help to visualise progression of the filter
    progressBar <- txtProgressBar(min = 1, max = nrow(data))
  }

  # particle filter
  for (i in seq_len(nrow(data))) {
    # initial + observation times
    times <- c(ifelse(i == 1, 0, data$time[i - 1]), data$time[i])
    dataPoint <- unlist(data[i, ]) # must be a vector

    if (!all(weightParticles == 0)) {
      # resample particles according to their weight (normalization is done in
      # the function sample())
      indexResampled <- sample(
        x = nParticles, size = nParticles, replace = TRUE,
        prob = weightParticles
      )
    } else {
      warning(
        "All particles depleted at step ", i, " of SMC. Return margLogLike = ",
        "-Inf for theta: ", printNamedVector(theta), call. = FALSE
      )
      return(list(margLogLike = -Inf, traj = NA, trajWeight = NA))
    }

    # update traj and current state after resampling
    trajParticles <- trajParticles[indexResampled]
    currentStateParticles <- currentStateParticles[indexResampled]

    # propagate particles (this for loop could be parallelized)
    propagate <- future_map(currentStateParticles, \(currentState) {
      # simulate from previous observation to current observation time
      traj <- fitmodel$simulate(
        theta = theta, initState = currentState, times = times
      )

      # compute particle weight
      modelPoint <- unlist(traj[2, fitmodel$stateNames])
      weight <- fitmodel$dPointObs(
        dataPoint = dataPoint, modelPoint = modelPoint, theta = theta
      )

      return(list(state = modelPoint, weight = weight))
    }, .options = furrr_options(seed = TRUE))

    # collect parallel jobs
    currentStateParticles <- future_map(propagate, \(x) {
      x$state
    })
    weightParticles <- unlist(future_map(propagate, \(x) {
      x$weight
    }))
    trajParticles <- future_map(seq_along(propagate), \(j) {
      rbind(trajParticles[[j]], c(dataPoint["time"], propagate[[j]]$state))
    })

    # update marginal log-likelihood
    margLogLike <- margLogLike + log(mean(weightParticles))

    if (progress) {
      # advance progress bar
      setTxtProgressBar(progressBar, i)
    }
  }

  if (progress) {
    close(progressBar)
  }

  # return marginal log-likelihood, filtered trajectories, normalised weight of
  # each trajectory
  ans <- list(
    margLogLike = margLogLike, traj = trajParticles,
    trajWeight = weightParticles / sum(weightParticles)
  )

  return(ans)
}
