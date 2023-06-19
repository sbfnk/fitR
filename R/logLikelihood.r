#' Log-likelihood of a trajectory for a deterministic model
#'
#' Compute the trajectory (log-)likelihood of \code{theta} for a
#' deterministic model defined in a \code{\link{fitmodel}} object by
#' summing the point log-likelihoods.
#' @inheritParams testFitmodel
#' @export
#' @seealso \code{\link{rTrajObs}}
#' @return numeric value of the log-likelihood
#' @param log logical (default: FALSE); whether the logarithm of the likelihood
#'   should be returned
dTrajObs <- function(fitmodel, theta, initState, data, log = TRUE) {
  # time sequence (must include initial time)
  times <- c(0, data$time)

  # simulate model at successive observation times of data
  traj <- fitmodel$simulate(theta, initState, times)

  dens <- 0

  # compute log-likelihood by summing the log-likelihood of each dataPoint
  for (i in seq_len(nrow(data))) {
    # extract dataPoint
    dataPoint <- unlist(data[i, ])

    # extract model point
    # we use i+1 since the first row of traj contains the initial state.
    modelPoint <- unlist(traj[i + 1, ])

    # update marginal log-likelihood
    dens <- dens +
      fitmodel$dPointObs(
        dataPoint = dataPoint, modelPoint = modelPoint, theta = theta,
        log = log
      )
  }

  return(ifelse(log, dens, exp(dens)))
}

#' Marginal log-likelihood for a stochastic model
#'
#' Compute a Monte-Carlo estimate of the log-likelihood of \code{theta} for a
#' stochastic model defined in a \code{\link{fitmodel}} object, using
#' \code{\link{particleFilter}}
#' @inheritParams testFitmodel
#' @inheritParams particleFilter
#' @export
#' @seealso particleFilter
#' @return Monte-Carlo estimate of the marginal log-likelihood of \code{theta}
margLogLikeSto <- function(fitmodel, theta, initState, data, nParticles) {
  # run SMC
  smc <- particleFilter(
    fitmodel = fitmodel, theta = theta, initState = initState, data = data,
    nParticles = nParticles
  )

  return(smc$margLogLike)
}

#' Posterior distribution for a fitmodel
#'
#' This function evaluates the posterior distribution at \code{theta} and
#' returns the result in a suitable format for \code{\link{mcmcMh}}.
#' @param margLogLike \R-function to compute the marginal log-likelihood of
#'   \code{theta}.
#' @param ... further arguments to be passed to \code{margLogLike}
#' @inheritParams testFitmodel
#' @export
#' @seealso \code{\link{dTrajObs}}, \code{\link{margLogLikeSto}}
#' @return a list of two elements
#' \itemize{
#' 	\item \code{logDensity} numeric, logged value of the posterior density
#'   evaluated at \code{theta}
#' 	\item \code{trace} named vector with trace information (theta,
#'   logPrior, margLogLike, logPosterior)
#' }
dLogPosterior <- function(fitmodel, theta, initState, data,
                          margLogLike = dTrajObs, ...) {
  logPrior <- fitmodel$dPrior(theta = theta, log = TRUE)

  if (is.finite(logPrior)) {
    logLikelihood <- margLogLike(
      fitmodel = fitmodel, theta = theta, initState = initState, data = data,
      ...
    )
  } else {
    ## do not compute log-likelihood (theta prior is 0)
    logLikelihood <- -Inf
  }

  logPosterior <- logPrior + logLikelihood

  return(list(
    logDensity = logPosterior,
    trace = c(
      theta,
      logPrior = logPrior,
      logLikelihood = logLikelihood,
      logPosterior = logPosterior
    )
  ))
}


#' A wrapper for \code{dLogPosterior}
#'
#' A wrapper for \code{\link{dLogPosterior}} that returns a function that can be
#' used as a \code{target} for \code{\link{mcmcMh}}
#' @inheritParams dLogPosterior
#' @export
#' @return a \R-function with one argument called \code{theta}.
#' @keywords internal
dLogPosteriorWrapper <- function(fitmodel, initState, data, margLogLike, ...) {
  function(theta) {
    dLogPosterior(fitmodel, theta, initState, data, margLogLike, ...)
  }
}


#' Generate an observation trajectory for a fitmodel
#'
#' This function simulates a model defined in a \code{\link{fitmodel}}
#' object and generates observations at each time point. It returns
#' the trajectory with an additions \code{obs} column.
#' @inheritParams testFitmodel
#' @param times the times at which to generate observations
#' @export
#' @seealso \code{\link{dTrajObs}}
#' @importFrom furrr future_map furrr_options
#' @importFrom dplyr bind_rows left_join
#' @return data.frame
rTrajObs <- function(fitmodel, theta, initState, times) {
  ## simulate model at successive observation times of data
  traj <- fitmodel$simulate(theta, initState, times)

  ## generate observations by applying fitmodel$rPointObs to
  ## each row of traj. The parameter value theta as passed as
  ## extra argument to fitmodel$rPointObs
  obs <- split(traj, f = traj$time)
  obs <- future_map(obs, \(x) {
    data.frame(
      time = unique(x$time),
      obs = fitmodel$rPointObs(x, theta = theta)
    )
  }, .options = furrr_options(seed = TRUE))
  obs <- bind_rows(obs)
  traj <- left_join(traj, obs, by = "time")

  return(traj)
}



#' Compute the DIC
#'
#' This function computes the Deviance Information Criterion (DIC) of a
#' \code{\link{fitmodel}} from a MCMC sample.
#' @param trace either a \code{data.frame} or \code{mcmc} object. Must contain
#'   one column with the posterior \code{logLikelihood}.
#' @inheritParams testFitmodel
#' @inheritParams dLogPosterior
#' @export
#' @importFrom stats var
#' @return a list of 5 elements:
#' \itemize{
#'     \item \code{DIC} value of the DIC
#'     \item \code{thetaBar} mean posterior of theta
#'     \item \code{logLikeThetaBar} log-likelihood of \code{thetaBar}
#'     \item \code{DThetaBar} deviance of \code{thetaBar}
#'     \item \code{pD} effective number of parameters
#' }
computeDic <-
  function(trace, fitmodel, initState, data, margLogLike = dTrajObs, ...) {
  simulation <- match.arg(simulation)

  # compute mean posterior estimate
  thetaBar <- colMeans(trace[fitmodel$thetaNames])

  logLikeThetaBar <- margLogLike(
    fitmodel, thetaBar, initState, data = data, ...
  )

  # and its deviance
  devThetaBar <- -2 * logLikeThetaBar

  # the effective number of parameters
  pD <- var(-2 * trace$logLikelihood) / 2

  # and finally the DIC
  dic <- devThetaBar + 2 * pD # nolint

  ans <- list(
    dic = dic, thetaBar = thetaBar, logLikeThetaBar = logLikeThetaBar,
    devThetaBar = devThetaBar, pD = pD
)

  return(ans)
}
