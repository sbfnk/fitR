#' Metropolis-Hasting MCMCda
#'
#' Run \code{nIterations} of a Metropolis-Hasting MCMC to sample from the
#' target distribution using a gaussian proposal kernel.
#' Two optional optimizations are also implemented: truncated gaussian proposal
#' (to match the support of the target distribution, i.e. boundary of the
#' parameters) and adaptative gaussian proposal (to match the size and the shape
#' of the target distribution).
#' @param target \R-function that takes a single argument: \code{theta} (named
#'   numeric vector of parameter values) and returns a list of 2 elements:
#' \itemize{
#' \item \code{logDensity} the logged value of the target density, evaluated at
#'   \code{theta}.
#' \item \code{trace} a named numeric vector of values to be printed in the
#'   \code{trace} data.frame returned by \code{mcmcMH}.
#' }
#' @param initTheta named vector of initial parameter values to start the
#'   chain.
#' @param proposalSD vector of standard deviations. If this is given and covmat
#'   is not, a diagonal matrix will be built from this to use as covariance
#'   matrix of the multivariate Gaussian proposal distribution. By default, this
#'   is set to \code{initTheta/10}.
#' @param nIterations number of iterations to run the MCMC chain.
#' @param covmat named numeric covariance matrix of the multivariate Gaussian
#'   proposal distribution. Must have named rows and columns with at least all
#'   estimated theta. If \code{proposalSD} is given, this is ignored.
#' @param limits limits for the - potentially truncated - multi-variate normal
#'   proposal distribution of the MCMC. Contains 2 elements:
#' \itemize{
#'      \item \code{lower} named numeric vector. Lower truncation points in each
#'   dimension of the Gaussian proposal distribution. By default they are set to
#'   \code{-Inf}.
#'      \item \code{upper} named numeric vector. Upper truncation points in each
#'   dimension of the Gaussian proposal distribution. By default they are set to
#'   \code{Inf}.
#' }
#' @param adaptSizeStart number of iterations to run before adapting the size
#'   of the proposal covariance matrix (see note below). Set to NULL (default) if
#'   size is not to be adapted.
#' @param adaptSizeCooling cooling factor for the scaling factor of the
#'   covariance matrix during size adaptation (see note below).
#' @param adaptShapeStart number of accepted jumps before adapting the shape
#'   of the proposal covariance matrix (see note below). Set to NULL (default) if
#'   shape is not to be adapted
#' @param adaptShapeStop number of iterations to run with adaptations
#'   of the shape of the proposal covariance matrix  before stopping. Se
#'   to NULL (default) if never to stop.
#' @param printInfoEvery frequency of information on the chain: acceptance
#'   rate and state of the chain. Default value to \code{nIterations/100}. Set
#'   to \code{NULL} to avoid any info.
#' @param verbose logical. If \code{TRUE}, information are printed.
#' @param maxScalingSD numeric. Maximum value for the scaling factor of the
#'   covariance matrix. Avoid too high values for the scaling factor, which
#'   might happen due to the exponential update scheme. In this case, the
#'   covariance matrix becomes too wide and the sampling from the truncated
#'   proposal kernel becomes highly inefficient
#' @note The size of the proposal covariance matrix is adapted using the
#'   following formulae: \deqn{\Sigma_{n+1}=\sigma_n * \Sigma_n} with
#'   \eqn{\sigma_n=\sigma_{n-1}*exp(\alpha^n*(acc - 0.234))}, where \eqn{\alpha}
#'   is equal to \code{adaptSizeCooling} and \eqn{acc} is the acceptance rate
#'   of the chain.
#'
#' The shape of the proposal covariance matrix is adapted using the following
#'   formulae: \deqn{\Sigma_{n+1}=2.38^2/d * \Sigma_n} with \eqn{\Sigma_n} the
#'   empirical covariance matrix and \eqn{d} is the number of estimated
#'   parameters in the model.
#' @references Roberts GO, Rosenthal JS. Examples of adaptive MCMC. Journal of
#'   Computational and Graphical Statistics. Taylor & Francis;
#'   2009;18(2):349-67.
#' @export
#' @importFrom lubridate as.period
#' @importFrom tmvtnorm rtmvnorm dtmvnorm
#' @importFrom stats runif
#' @return a list with 3 elements:
#' \itemize{
#'      \item \code{trace} a \code{data.frame}. Each row contains a state of the
#'   chain (as returned by \code{target}, and an extra column for the
#'   logDensity).
#'      \item \code{acceptanceRate} acceptance rate of the MCMC chain.
#'      \item \code{covmatProposal} last covariance matrix used for proposals.
#' }
mcmcMH <- function(
    target, initTheta, proposalSD = NULL,
    nIterations, covmat = NULL,
    limits = list(lower = NULL, upper = NULL),
    adaptSizeStart = NULL, adaptSizeCooling = 0.99,
    adaptShapeStart = NULL, adaptShapeStop = NULL,
    printInfoEvery = nIterations / 100,
    verbose = FALSE, maxScalingSD = 50) {
  # initialise theta
  thetaCurrent <- initTheta
  thetaPropose <- initTheta

  # extract theta of gaussian proposal
  covmatProposal <- covmat
  lowerProposal <- limits$lower
  upperProposal <- limits$upper

  # reorder vector and matrix by names, set to default if necessary
  thetaNames <- names(initTheta)
  if (!is.null(proposalSD) && is.null(names(proposalSD))) {
    names(proposalSD) <- thetaNames
  }

  if (is.null(covmatProposal)) {
    if (is.null(proposalSD)) {
      proposalSD <- initTheta / 10
    }
    covmatProposal <-
      matrix(diag(proposalSD[thetaNames]^2, nrow = length(thetaNames)),
        nrow = length(thetaNames),
        dimnames = list(thetaNames, thetaNames)
      )
  } else {
    covmatProposal <- covmatProposal[thetaNames, thetaNames]
  }

  if (is.null(lowerProposal)) {
    lowerProposal <- initTheta
    lowerProposal[] <- -Inf
  } else {
    lowerProposal <- lowerProposal[thetaNames]
  }

  if (is.null(upperProposal)) {
    upperProposal <- initTheta
    upperProposal[] <- Inf
  } else {
    upperProposal <- upperProposal[thetaNames]
  }

  # covmat init
  covmatProposalInit <- covmatProposal

  adaptingSize <- FALSE # will be set to TRUE once we start
  # adapting the size

  adaptingShape <- 0 # will be set to the iteration at which
  # adaptation starts

  # find estimated theta
  thetaEstimatedNames <- names(which(diag(covmatProposal) > 0))

  # evaluate target at theta init
  targetThetaCurrent <- target(thetaCurrent)

  if (!is.null(printInfoEvery)) {
    message(
      Sys.time(), ", Init: ",
      printNamedVector(thetaCurrent[thetaEstimatedNames]),
      ", target: ", targetThetaCurrent
    )
  }

  # trace
  trace <- matrix(ncol = length(thetaCurrent) + 1, nrow = nIterations, 0)
  colnames(trace) <- c(thetaEstimatedNames, "logDensity")

  # acceptance rate
  acceptanceRate <- 0

  # scaling factor for covmat size
  scalingSD <- 1

  # scaling multiplier
  scalingMultiplier <- 1

  # empirical covariance matrix (0 everywhere initially)
  covmatEmpirical <- covmatProposal
  covmatEmpirical[, ] <- 0

  # empirical mean vector
  thetaMean <- thetaCurrent

  # if printInfoEvery is null never print info
  if (is.null(printInfoEvery)) {
    printInfoEvery <- nIterations + 1
  }

  for (iIteration in seq_len(nIterations)) {
    # adaptive step
    if (!is.null(adaptSizeStart) && iIteration >= adaptSizeStart &&
      (is.null(adaptShapeStart) ||
       acceptanceRate * iIteration < adaptShapeStart)) {
      if (!adaptingSize) {
        message("\n---> Start adapting size of covariance matrix")
        adaptingSize <- TRUE
      }
      # adapt size of covmat until we get enough accepted jumps
      scalingMultiplier <-
        exp(adaptSizeCooling^(iIteration - adaptSizeStart) *
            (acceptanceRate - 0.234))
      scalingSD <- scalingSD * scalingMultiplier
      scalingSD <- min(c(scalingSD, maxScalingSD))
      # only scale if it doesn't reduce the covariance matrix to 0
      covmatProposalNew <- scalingSD^2 * covmatProposalInit
      if (!(any(diag(covmatProposalNew)[thetaEstimatedNames] <
        .Machine$double.eps))) {
        covmatProposal <- covmatProposalNew
      }
    } else if (!is.null(adaptShapeStart) &&
      acceptanceRate * iIteration >= adaptShapeStart &&
      (adaptingShape == 0 || is.null(adaptShapeStop) ||
        iIteration < adaptingShape + adaptShapeStop)) {
      if (!adaptingShape) {
        message("\n---> Start adapting shape of covariance matrix")
        adaptingShape <- iIteration
      }

      ## adapt shape of covmat using optimal scaling factor for multivariate
      ## target distributions
      scalingSD <- 2.38 / sqrt(length(thetaEstimatedNames))

      covmatProposal <- scalingSD^2 * covmatEmpirical
    } else if (adaptingShape > 0) {
      message("\n---> Stop adapting shape of covariance matrix")
      adaptingShape <- -1
    }

    # print info
    if (iIteration %% ceiling(printInfoEvery) == 0) {
      message(Sys.time(), ", Iteration: ", iIteration, "/", nIterations,
        ", acceptance rate: ",
        sprintf("%.3f", acceptanceRate),
        appendLF = FALSE
      )
      if (!is.null(adaptSizeStart) || !is.null(adaptShapeStart)) {
        message(", scalingSD: ", sprintf("%.3f", scalingSD),
          ", scalingMultiplier: ", sprintf("%.3f", scalingMultiplier),
          appendLF = FALSE
        )
      }
      message(", state: ", (printNamedVector(thetaCurrent)))
      message(", logdensity: ", targetThetaCurrent)
    }

    # propose another parameter set
    if (any(diag(covmatProposal)[thetaEstimatedNames] <
      .Machine$double.eps)) {
      print(covmatProposal[thetaEstimatedNames, thetaEstimatedNames])
      stop("non-positive definite covmat", call. = FALSE)
    }
    if (length(thetaEstimatedNames) > 0) {
      thetaPropose[thetaEstimatedNames] <-
        as.vector(rtmvnorm(1,
          mean =
            thetaCurrent[thetaEstimatedNames],
          sigma =
            covmatProposal[thetaEstimatedNames, thetaEstimatedNames],
          lower =
            lowerProposal[thetaEstimatedNames],
          upper = upperProposal[thetaEstimatedNames]
        ))
    }

    # evaluate posterior of proposed parameter
    targetThetaPropose <- target(thetaPropose)
    # if return value is a vector, set logDensity and trace

    if (!is.finite(targetThetaPropose)) {
      # if posterior is 0 then do not compute anything else and don't accept
      logAcceptance <- -Inf
    } else {
      # compute Metropolis-Hastings ratio (acceptance probability)
      logAcceptance <- targetThetaPropose - targetThetaCurrent
      logAcceptance <- logAcceptance +
        dtmvnorm(
          x = thetaCurrent[thetaEstimatedNames],
          mean =
            thetaPropose[thetaEstimatedNames],
          sigma =
            covmatProposal[
              thetaEstimatedNames,
              thetaEstimatedNames
            ],
          lower =
            lowerProposal[thetaEstimatedNames],
          upper =
            upperProposal[thetaEstimatedNames],
          log = TRUE
        )
      logAcceptance <- logAcceptance -
        dtmvnorm(
          x = thetaPropose[thetaEstimatedNames],
          mean = thetaCurrent[thetaEstimatedNames],
          sigma =
            covmatProposal[
              thetaEstimatedNames,
              thetaEstimatedNames
            ],
          lower =
            lowerProposal[thetaEstimatedNames],
          upper =
            upperProposal[thetaEstimatedNames],
          log = TRUE
        )
    }

    if (verbose) {
      message("Propose: ", thetaPropose[thetaEstimatedNames],
        ", target: ", targetThetaPropose,
        ", acc prob: ", exp(logAcceptance), ", ",
        appendLF = FALSE
      )
    }

    if (isAccepted <- (log(runif(1)) < logAcceptance)) {
      # accept proposed parameter set
      thetaCurrent <- thetaPropose
      targetThetaCurrent <- targetThetaPropose
      if (verbose) {
        message("accepted")
      }
    } else if (verbose) {
      message("rejected")
    }
    trace[iIteration, ] <- c(thetaCurrent, targetThetaCurrent)

    # update acceptance rate
    if (iIteration == 1) {
      acceptanceRate <- isAccepted
    } else {
      acceptanceRate <- acceptanceRate +
        (isAccepted - acceptanceRate) / iIteration
    }

    # update empirical covariance matrix
    if (adaptingShape >= 0) {
      tmp <- updateCovmat(
        covmatEmpirical, thetaMean,
        thetaCurrent, iIteration
      )
      covmatEmpirical <- tmp$covmat
      thetaMean <- tmp$thetaMean
    }
  }

  return(list(
    trace = trace,
    acceptanceRate = acceptanceRate,
    covmatEmpirical = covmatEmpirical
  ))
}
