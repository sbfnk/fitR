#' Simulate forward a stochastic model
#'
#' This function uses the function \code{\link[adaptivetau]{ssa.adaptivetau}} to
#' simulate the model and returns the trajectories in a valid format for the
#' class \code{\link{fitmodel}}.
#' @param theta named vector of model parameters.
#' @param initState named vector of initial state of the model.
#' @param times time sequence for which state of the model is wanted; the first
#'   value of times must be the initial time.
#' @inheritParams adaptivetau::ssa.adaptivetau
#' @export
#' @importFrom adaptivetau ssa.adaptivetau
#' @importFrom stats approx
#' @return a data.frame of dimension \code{length(times)x(length(initState)+1)}
#'   with column names equal to \code{c("time",names(initState))}.
simulateModelStochastic <- function(theta, initState, times, transitions,
                                    rateFunc) {
  stoch <- as.data.frame(ssa.adaptivetau(
    initState, transitions, rateFunc, theta, tf = diff(range(times))
  ))

  # rescale time as absolute value
  stoch$time <- stoch$time + min(times)

  # interpolate
  traj <- cbind(time = times, apply(stoch[, -1], 2, function(col) {
    approx(x = stoch[, 1], y = col, xout = times, method = "constant")$y
  }))

  return(as.data.frame(traj))
}



#' Simulate several replicate of the model
#'
#' Simulate several replicate of a fitmodel using its function simulate
#' @param times vector of times at which you want to observe the states of the
#'   model.
#' @param n number of replicated simulations.
#' @param observation logical, if \code{TRUE} simulated observation are
#'   generated by \code{rTrajObs}.
#' @inheritParams testFitmodel
#' @importFrom furrr future_map future_options
#' @importFrom dplyr bind_rows
#' @export
#' @return a data.frame of dimension
#'   \code{[nxlength(times)]x[length(initState)+2]} with column names equal to
#'   \code{c("replicate","time",names(initState))}.
simulateModelReplicates <- function(fitmodel, theta, initState, times, n,
                                    observation = FALSE) {
  stopifnot(inherits(fitmodel, "fitmodel"), n > 0)

  if (observation && is.null(fitmodel$dPointObs)) {
    stop(
      "Can't generate observation as ", sQuote("fitmodel"), " doesn't have a ",
      sQuote("dPointObs"), " function."
    )
  }

  rep <- as.list(1:n)
  names(rep) <- rep

  progress <- (n > 1)

  trajRep <- future_map(rep, function(x) {
    if (observation) {
      traj <- rTrajObs(fitmodel, theta, initState, times)
    } else {
      traj <- fitmodel$simulate(theta, initState, times)
    }

    return(traj)
  }, .progress = progress, .options = furrr_options(seed = TRUE))
  trajRep <- bind_rows(trajRep, .id = "replicate")

  return(trajRep)
}


#' Simulate model until extinction
#'
#' Return final state at extinction
#' @param extinct character vetor. Simulations stop when all these state are
#'   extinct.
#' @param timeInit numeric. Start time of simulation.
#' @param timeStep numeric. Time step at which extinction is checked
#' @inheritParams testFitmodel
#' @inheritParams simulateModelReplicates
#' @inheritParams particleFilter
#' @export
#' @importFrom future plan
#' @importFrom furrr future_map
#' @importFrom dplyr last bind_rows
#' @keywords internal
simulateFinalStateAtExtinction <- function(fitmodel, theta, initState,
                                           extinct = NULL, timeInit = 0,
                                           timeStep = 100, n = 100,
                                           observation = FALSE, nCores = 1) {
  stopifnot(inherits(fitmodel, "fitmodel"), n > 0)

  if (observation && is.null(fitmodel$dPointObs)) {
    stop(
      "Can't generate observation as ", sQuote("fitmodel"), " doesn't have a ",
      sQuote("dPointObs"), " function."
    )
  }

  rep <- as.list(1:n)
  names(rep) <- rep

  if ((is.null(nCores) > 1 || is.null(nCores)) &&
      inherits(plan(), "sequential")) {
    warning(
      "Parallel processing is disabled. To enable, call `future::plan` ",
      "with a parallel strategy, e.g. `future::plan(\"multisession\")`. ",
      "For more details, read the corresponding manual page using ",
      "`?future::plan`."
    )
  }

  progress <- (n > 1 && nCores == 1)

  times <- c(timeInit, timeStep) # nolint: object_usage_linter

  finalStateRep <- future_map(
    rep, function(x) {
      if (observation) {
        traj <- rTrajObs(fitmodel, theta, initState, times)
      } else {
        traj <- fitmodel$simulate(theta, initState, times)
      }

      currentState <- unlist(traj[nrow(traj), fitmodel$stateNames])
      currentTime <- last(traj$time)

      while (any(currentState[extinct] >= 0.5)) {
        times <- times + currentTime

        if (observation) {
          traj <- rTrajObs(fitmodel, theta, currentState, times)
        } else {
          traj <- fitmodel$simulate(theta, currentState, times)
        }

        currentState <- unlist(traj[nrow(traj), fitmodel$stateNames])
        currentTime <- last(traj$time)
      }

      return(data.frame(t(c(time = currentTime, currentState))))
    }, .progress = progress
  )
  finalStateRep <- bind_rows(finalStateRep, .id = "replicate")

  return(finalStateRep)
}


#' Update covariance matrix
#'
#' Update covariance matrix using a stable one-pass algorithm. This is much more
#' efficient than using \code{\link{cov}} on the full data.
#' @param covmat covariance matrix at iteration \code{i-1}. Must be numeric,
#'   symmetrical and named.
#' @param thetaMean mean vector at iteration \code{i-1}. Must be numeric and
#'   named.
#' @param theta vector of new value at iteration \code{i}. Must be numeric and
#'   named.
#' @param i current iteration.
#' @references \url{http://en.wikipedia.org/wiki/Algorithms\%5Ffor\%5Fcalculating\%5Fvariance#Covariance} # nolint
#' @export
#' @keywords internal
#' @return A list of two elements
#' \itemize{
#' \item \code{covmat} update covariance matrix
#' \item \code{thetaMean} updated mean vector
#' }
updateCovmat <- function(covmat, thetaMean, theta, i) {
  if (is.null(names(theta))) {
    stop("Argument ", sQuote("theta"), " must be named.", .call = FALSE)
  }
  if (is.null(names(thetaMean))) {
    stop("Argument ", sQuote("thetaMean"), " must be named.", .call = FALSE)
  }
  if (is.null(rownames(covmat))) {
    stop("Argument ", sQuote("covmat"), " must have named rows.", .call = FALSE)
  }
  if (is.null(colnames(covmat))) {
    stop(
      "Argument ", sQuote("covmat"), " must have named columns.", .call = FALSE
    )
  }

  covmat <- covmat[names(theta), names(theta)]
  thetaMean <- thetaMean[names(theta)]

  residual <- as.vector(theta - thetaMean)
  covmat <- (covmat * (i - 1) + (i - 1) / i * residual %*% t(residual)) / i
  thetaMean <- thetaMean + residual / i

  return(list(covmat = covmat, thetaMean = thetaMean))
}



#' Burn and thin MCMC chain
#'
#' Return a burned and thined trace of the chain.
#' @param trace either a \code{data.frame} or a \code{list} of \code{data.frame}
#'   with all variables in column, as outputed by \code{\link{mcmcMh}}. Accept
#'   also an \code{mcmc} or \code{mcmc.list} object.
#' @param burn proportion of the chain to burn.
#' @param thin number of samples to discard per sample that is being kept
#' @export
#' @importFrom coda mcmc mcmc.list
#' @return an object with the same format as \code{trace} (\code{data.frame} or
#'   \code{list} of \code{data.frame} or \code{mcmc} object or \code{mcmc.list}
#'   object)
#' @examples
#' data(mcmcEpi)
#' burnAndThin(mcmcEpi1$trace, burn = 100, thin = 4)
burnAndThin <- function(trace, burn = 0, thin = 0) {
  convertToMCMC <- FALSE

  if (inherits(trace, "mcmc")) {
    convertToMCMC <- TRUE
    trace <- as.data.frame(trace)
  } else if (inherits(trace, "mcmc.list")) {
    convertToMCMC <- TRUE
    trace <- as.list(trace)
  }

  if (is.data.frame(trace) || is.matrix(trace)) {
    # remove burn
    if (burn > 0) {
      trace <- trace[-(1:burn), ]
    }
    # thin
    trace <- trace[seq(1, nrow(trace), thin + 1), ]

    if (convertToMCMC) {
      trace <- mcmc(trace)
    }
  } else {
    trace <- lapply(trace, function(x) {
      # remove burn
      if (burn > 0) {
        x <- x[-(1:burn), ]
      }
      # thin
      x <- x[seq(1, nrow(x), thin + 1), ]

      if (convertToMCMC) {
        x <- mcmc(x)
      }

      return(x)
    })

    if (convertToMCMC) {
      trace <- mcmc.list(trace)
    }
  }

  return(trace)
}


#' Distance weighted by number of oscillations
#'
#' This positive distance is the mean squared differences between \code{x} and
#' the \code{y}, divided by the square of the number of times the \code{x}
#' oscillates around the \code{y} (see note below for illustration).
#' @param x,y numeric vectors of the same length.
#' @note To illustrate this distance, suppose we observed a time series \code{y
#'   = c(1,3,5,7,5,3,1)} and we have two simulated time series \code{x1 =
#'   (3,5,7,9,7,5,3)} and \code{x2 = (3,5,3,5,7,5,3)}; \code{x1} is consistently
#'   above \code{y} and \code{x2} oscillates around \code{y}. While the squared
#'   differences are the same, we obtain \eqn{d(y, x1) = 4} and \eqn{d(y, x2) =
#'   1.3}.
#' @export
distanceOscillation <- function(x, y) {
  # check x and y have same length
  if (length(x) != length(y)) {
    stop(
      sQuote("x"), " and ", sQuote("y"), " must be vector of the same length"
    )
  }

  # 1 + number of times x oscillates around y
  nOscillations <- 1 + length(which(diff((x - y) > 0) != 0))

  dist <- sum((x - y)^2) / (length(x) * nOscillations)

  return(dist)
}


#' Export trace in Tracer format
#'
#' Print \code{trace} in a \code{file} that can be read by the software Tracer.
#' @param trace a \code{data.frame} with one column per estimated parameter, as
#'   returned by \code{\link{burnAndThin}}
#' @inheritParams utils::write.table
#' @note Tracer is a program for analysing the trace files generated by Bayesian
#'   MCMC runs. It can be dowloaded at
#'   \url{http://tree.bio.ed.ac.uk/software/tracer}.
#' @importFrom utils write.table
#' @export
#' @seealso burnAndThin
#' @keywords internal
export2Tracer <- function(trace, file) {
  if (is.mcmc(trace)) {
    trace <- as.data.frame(trace)
  }

  if (!"iteration" %in% names(trace)) {
    trace$iteration <- seq_len(nrow(trace)) - 1
  }

  trace <- trace[
    c("iteration", setdiff(names(trace), c("iteration", "weight")))
  ]
  write.table(trace, file = file, quote = FALSE, row.names = FALSE, sep = "\t")
}


#' Print named vector
#'
#' Print named vector with format specified by \code{fmt} (2 decimal places by
#' default).
#' @param x named vector
#' @inheritParams base::sprintf
#' @inheritParams base::paste
#' @export
#' @seealso \code{\link[base]{sprintf}}
#' @keywords internal
printNamedVector <- function(x, fmt = "%.2f", sep = " | ") {
  paste(paste(names(x), sprintf(fmt, x), sep = " = "), collapse = sep)
}
