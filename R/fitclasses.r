#' Constructor of fitmodel object
#'
#' A \code{fitmodel} object is a \code{list} that stores some variables and
#' functions that will be useful to simulate and fit your model during the
#' course.
#' @param name character. Name of the model (required).
#' @param stateNames character vector. Names of the state variables i.e.
#'   \code{c("S","I","R")} (required).
#' @param thetaNames character vector. Names of the parameters i.e.
#'   \code{c("R_0","infectiousPeriod")} (required).
#' @param simulate \R-function to simulate forward the model (required). This
#'   function takes 3 arguments:
#' \itemize{
#' \item \code{theta} named numeric vector. Values of the parameters. Names
#'   should match \code{thetaNames}.
#' \item \code{initState} named numeric vector. Initial values of the state
#'   variables. Names should match \code{stateNames}.
#' \item \code{times} numeric vector. Time sequence for which the state of the
#'   model is wanted; the first value of times must be the initial time, i.e.
#'   the time of \code{initState}.
#' }
#' and returns a \code{data.fame} containing the simulated trajectories that is
#'   the values of the state variables (1 per column) at each observation time
#'   (1 per row). The first column is \code{time}.
#' @param rPointObs \R-function that generates a (randomly sampled) observation
#'   point from a model point, using an observation model (optional). It thus
#'   acts as an inverse of \code{dPointObs} (see below). This function takes 2
#'   arguments
#' \itemize{
#' \item \code{modelPoint} named numeric vector. State of the model at a given
#'   point in time.
#' \item \code{theta} named numeric vector. Values of the parameters. Names
#'   should match \code{thetaNames}.
#' }
#' and returns an observation point
#' @param dprior \R-function that evaluates the prior density of the parameters
#'   at a given \code{theta} (optional). The function should take 2 arguments:
#' \itemize{
#' 	\item \code{theta} named numeric vector. Values of the parameters. Names
#'   should match \code{thetaNames}.
#' 	\item \code{log} boolean. determines whether the logarithm of the prior
#'   density should be returned.
#' }
#' and returns the (logged, if requested) value of the prior density
#'   distribution.
#' @param dPointObs \R-function that evaluates the likelihood of one data point
#'   given the state of the model at the same time point. This function takes 4
#'   arguments:
#' \itemize{
#' \item \code{dataPoint} named numeric vector. Observation time and observed
#'   data point.
#' \item \code{modelPoint} named numeric vector containing the state of the
#'   model at the observation time point.
#' \item \code{theta} named numeric vector. Parameter values. Useful since
#'   parameters are usually needed to compute the likelihood (i.e. reporting
#'   rate).
#' \item \code{log} boolean. determines whether the logarithm of the likelihood
#'   should be returned.
#' }
#' and returns the (log-)likelihood. (optional)
#' @export
#' @return a \code{fitmodel} object that is a \code{list} of 7 elements:
#' \itemize{
#' 	\item \code{name} character, name of the model
#' 	\item \code{stateNames} vector, names of the state variables.
#' 	\item \code{thetaNames} vector, names of the parameters.
#' 	\item \code{simulate} \R-function to simulate forward the model; usage:
#'   \code{simulate(theta,initState,times)}.
#' 	\item \code{rPointObs} \R-function to generate simulated observations;
#'   usage: \code{rPointObs(modelPoint, theta)}.
#' 	\item \code{dprior} \R-function to evaluate the log-prior of the parameter
#'   values; usage: \code{dprior(theta)}.
#' 	\item \code{dPointObs} \R-function to evaluate the log-likelihood of one
#'   data point; usage: \code{dPointObs(dataPoint, modelPoint, theta, log)}.
#' }
#' @seealso \code{\link{testFitmodel}}
#' @example inst/examples/example-SIR-deter.r
fitmodel <- function(name = NULL, stateNames = NULL, thetaNames = NULL,
                     simulate = NULL, rPointObs = NULL, dprior = NULL,
                     dPointObs = NULL) {
  # mandatory
  if (!is.character(name)) {
    stop(sQuote("name"), " argument is not a character")
  }
  if (!is.character(stateNames)) {
    stop(sQuote("stateNames"), " argument is not a character vector")
  }
  if (!is.character(thetaNames)) {
    stop(sQuote("thetaNames"), " argument is not a character vector")
  }
  if (!is.function(simulate)) {
    stop(sQuote("simulate"), " argument is not an R function")
  }

  # optional
  if (!is.null(rPointObs) && !is.function(rPointObs)) {
    stop(sQuote("rPointObs"), " argument is not an R function")
  }
  if (!is.null(dprior) && !is.function(dprior)) {
    stop(sQuote("dprior"), " argument is not an R function")
  }
  if (!is.null(dPointObs) && !is.function(dPointObs)) {
    stop(sQuote("dPointObs"), " argument is not an R function")
  }

  # create and return object
  return(structure(list(
    name = name,
    stateNames = stateNames,
    thetaNames = thetaNames,
    simulate = simulate,
    rPointObs = rPointObs,
    dprior = dprior,
    dPointObs = dPointObs
  ), class = "fitmodel"))
}

#' Test a fitmodel
#'
#' This function performs a serie of checks on the \code{\link{fitmodel}}
#' provided by the user in order to make sure that it will be compatible both
#' with the functions coded during the course and the functions available in the
#' \code{fitR} package. The latters can be used as a correction.
#' @param fitmodel a \code{\link{fitmodel}} object
#' @param theta named numeric vector. Values of the parameters. Names should
#'   match \code{fitmodel$thetaNames}.
#' @param initState named numeric vector. Initial values of the state
#'   variables. Names should match \code{fitmodel$stateNames}.
#' @param data data frame. Observation times and observed data. The time column
#'   must be named \code{"time"} and the observation column must be named
#'   \code{"obs"}.
#' @param verbose if \code{TRUE}, print details of the test performed to check
#'   validity of the arguments
#' @export
#' @seealso \code{\link{fitmodel}}
#' @example inst/examples/example-SIR-deter.r
# nolint start: cyclocomp_linter
testFitmodel <- function(fitmodel, theta, initState, data = NULL,
                         verbose = TRUE) {
  if (missing(fitmodel)) {
    stop(sQuote("fitmodel"), " argument missing\n")
  }
  if (!inherits(fitmodel, "fitmodel")) {
    stop(sQuote("fitmodel"), " argument is not from the class fitmodel")
  }

  ## test of theta
  if (missing(theta)) {
    stop(sQuote("theta"), " argument missing\n")
  }
  if (verbose) {
    cat("--- checking ", sQuote("theta"), "argument\n")
    cat(
      "Should contain the parameters:", sQuote(fitmodel$thetaNames),
      "\nTest:\n"
    )
    print(theta)
  }
  if (length(x <- setdiff(fitmodel$thetaNames, names(theta)))) {
    stop(
      "The following parameters are missing in argument ", sQuote("theta"),
      ": ", sQuote(x), call. = FALSE
    )
  }
  if (length(x <- setdiff(names(theta), fitmodel$thetaNames))) {
    stop(
      "The following parameters are not in ", sQuote("fitmodel$thetaNames"),
      ": ", sQuote(x), call. = FALSE
    )
  }
  if (verbose) {
    cat("--> ", sQuote("theta"), " argument looks good!\n")
  }

  ## test of initState
  if (missing(initState)) {
    stop(sQuote("initState"), " argument missing\n")
  }
  if (verbose) {
    cat("--- checking ", sQuote("initState"), "argument\n")
    cat("Should contain the states:", sQuote(fitmodel$stateNames), "\nTest:\n")
    print(initState)
  }
  if (length(x <- setdiff(fitmodel$stateNames, names(initState)))) {
    stop(
      "The following states are missing in argument ", sQuote("initState"),
       ": ", sQuote(x), call. = FALSE
    )
  }
  if (length(x <- setdiff(names(initState), fitmodel$stateNames))) {
    stop(
      "The following states are not in ", sQuote("fitmodel$stateNames"), ": ",
      sQuote(x), call. = FALSE
    )
  }
  if (verbose) {
    cat("--> ", sQuote("initState"), " argument looks good!\n")
  }

  testTraj <- NULL

  ## check simulate
  if (!is.null(fitmodel$simulate)) {
    if (verbose) {
      cat("--- checking simulate\n")
    }
    ## check arguments
    funArgs <- c("theta", "initState", "times")
    if (!(all(x <- funArgs %in% names(formals(fitmodel$simulate))))) {
      stop(
        "argument(s) ", sQuote(funArgs[!x]),
        " missing in function simulate, see ?fitmodel."
      )
    }

    if (!is.null(initState)) {
      times <- 0:10
      testTraj <- fitmodel$simulate(
        theta = theta, initState = initState, times = times
      )
      # must return a data.frame of dimension 11x(length(stateNames)+1)
      if (verbose) {
        cat(
          "simulate(theta, initState, times=0:10) should return a",
          "non-negative data.frame of dimension", length(times), "x",
          length(fitmodel$stateNames) + 1, "with column names:",
          sQuote(c("time", fitmodel$stateNames)), "\nTest:\n"
        )
        print(testTraj)
      }
      if (!is.data.frame(testTraj)) {
        stop("simulate must return a data.frame")
      }
      if (!all(x <- c("time", fitmodel$stateNames) %in% names(testTraj))) {
        stop(
          "Column(s) missing in the data.frame returned by simulate: ",
          sQuote(c("time", fitmodel$stateNames)[!x])
        )
      }
      if (!all(x <- names(testTraj) %in% c("time", fitmodel$stateNames))) {
        warning(
          "The following columns are not required in the data.frame returned ",
          "by simulate: ", sQuote(names(testTraj)[!x])
        )
      }
      if (any(testTraj$time != times)) {
        stop(
          "The time column of the data.frame returned by simulate is ",
          "different from its times argument", call. = FALSE
        )
      }
      if (any(testTraj < 0)) {
        stop(
          "simulate returned negative values during the test, use verbose ",
          "argument of fitmodel to check"
        )
      }
      if (verbose) {
        cat("--> simulate looks good!\n")
      }
    } else {
      warning("initState not given, not creating test trajectory\n")
    }
  } else {
    warning("fitmodel does not contain a simulate method -- not tested\n")
  }

  ## check rPointObs
  if (!is.null(fitmodel$rPointObs)) {
    if (verbose) {
      cat("--- checking rPointObs\n")
    }
    ## check arguments
    funArgs <- c("modelPoint", "theta")
    if (!(all(x <- funArgs %in% names(formals(fitmodel$rPointObs))))) {
      stop(
        "argument(s) ", sQuote(funArgs[!x]), " missing in function ",
        "rPointObs, see ?fitmodel."
      )
    }

    if (!is.null(testTraj)) {
      testRPointObs <- fitmodel$rPointObs(unlist(testTraj[1, ]), theta)
      if (verbose) {
        cat("rPointObs(testTraj, theta) should return a number\nTest:\n")
        print(testRPointObs)
      }
      if (!is.numeric(testRPointObs)) {
        stop("rPointObs must return a number")
      }
      if (testRPointObs < 0) {
        stop(
          "rPointObs returned negative observation during the test, use ",
          "verbose argument of fitmodel to check"
        )
      }
      if (verbose) {
        cat("--> rPointObs looks good!\n")
      }
    } else {
      warning("no test trajectory created, not creating test observation\n")
    }
  }


  if (!is.null(fitmodel$dprior)) {
    if (verbose) {
      cat("--- checking dprior\n")
    }

    # check arguments
    funArgs <- c("theta", "log")
    if (!(all(x <- funArgs %in% names(formals(fitmodel$dprior))))) {
      stop(
        "arguments ", sQuote(funArgs[!x]), " missing in function dprior, ",
        "see ?fitmodel."
      )
    }

    # test it
    testDprior <- fitmodel$dprior(theta)
    if (verbose) {
      cat(
        "dprior(theta) should return a single finite value\nTest:",
        testDprior, "\n"
      )
    }
    if (!(!is.na(testDprior) && (is.finite(testDprior)))) {
      stop("dprior must return a finite value for test parameter values")
    }
    if (verbose) {
      cat("--> dprior looks good!\n")
    }
  } else {
    warning("fitmodel does not contain a dprior method -- not tested\n")
  }

  # data must have a column named time, should not start at 0
  if (!is.null(data)) {
    if (!all(c("time", "obs") %in% names(data))) {
      stop(
        sQuote("data"), " argument must have columns named ", sQuote("time"),
        " and ", sQuote("obs")
      )
    } else if (data$time[1] == 0) {
      stop("the first observation time in data argument should not be 0")
    }
  }

  ## check dPointObs
  ## check arguments, return value
  if (!is.null(fitmodel$dPointObs)) {
    if (verbose) {
      cat("--- checking dPointObs\n")
    }
    # check arguments
    funArgs <- c("dataPoint", "modelPoint", "theta", "log")
    if (!(all(x <- funArgs %in% names(formals(fitmodel$dPointObs))))) {
      stop(
        "argument(s) ", sQuote(funArgs[!x]), " missing in function ",
        "dPointObs, see documentation."
      )
    }

    if (!is.null(data)) {
      if (!is.null(testTraj)) {
        ## test it, first data point corresponds to second simulation step
        ## (first row contain initial state)
        dataPoint <- unlist(data[1, ])
        modelPoint <- unlist(testTraj[2, ])
        testDPointObs <- fitmodel$dPointObs(
          dataPoint = dataPoint, modelPoint = modelPoint, theta = theta,
          log = TRUE
        )

        if (verbose) {
          cat(
            "dPointObs(dataPoint,modelPoint,theta) should return a single",
            "value\nTest:", testDPointObs, "\n"
          )
        }
        if (length(testDPointObs) > 1 || is.na(testDPointObs) ||
              (testDPointObs > 0)) {
          stop("dPointObs must return a single non-positive value")
        }
        if (verbose) {
          cat("--> dPointObs looks good!\n")
        }
      } else {
        warning("no test trajectory created, not creating test observation\n")
      }
    } else {
      warning("data argument not given -- not testing dPointObs function")
    }
  } else {
    warning("fitmodel does not contain a dPointObs method -- not tested\n")
  }
}
# nolint end: cyclocomp_linter
