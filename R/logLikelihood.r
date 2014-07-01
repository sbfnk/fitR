#' Log-likelihood of a trajectory for a deterministic model
#'
#' Compute the trajectory log-likelihood of \code{theta} for a
#' deterministic model defined in a \code{\link{fitmodel}} object by
#' summing the point log-likelihoods.
#' @inheritParams testFitmodel
#' @export
#' @seealso \code{\link{genObsTraj}}
#' @return numeric value of the log-likelihood
trajLogLike <- function(fitmodel, theta, state.init, data) {

	# time sequence (must include initial time)
	times <- c(0,data$time)

	# simulate model at successive observation times of data
	traj <- fitmodel$simulate(theta,state.init,times)

        logLike <- 0

        # compute log-likelihood by summing the log-likelihood of each data point
	for(i in 1:nrow(data)){

		# extract data point
		data.point <- unlist(data[i,])

		# extract model point
		# we use i+1 since the first row of traj contains the initial state.
		model.point <- unlist(traj[i+1,fitmodel$state.names])

		# update marginal log-likelihood
		logLike <- logLike + fitmodel$pointLogLike(data.point=data.point, model.point=model.point, theta=theta)
	}

	return(logLike)
}

#'Marginal log-likelihood for a stochastic model
#'
#'Compute a Monte-Carlo estimate of the log-likelihood of \code{theta} for a stochastic model defined in a \code{\link{fitmodel}} object, using \code{\link{particleFilter}}
#' @inheritParams testFitmodel
#' @inheritParams particleFilter
#' @export
#' @seealso particleFilter
#' @return Monte-Carlo estimate of the marginal log-likelihood of \code{theta}
margLogLikeSto <- function(fitmodel, theta, state.init, data, n.particles, n.cores = 1) {

	# run SMC
	smc <- particleFilter(fitmodel=fitmodel, theta=theta, state.init=state.init, data=data, n.particles=n.particles, n.cores=n.cores)

	return(smc$margLogLike)
}

#'Posterior distribution for a fitmodel
#'
#'This function evaluates the posterior distribution at \code{theta} and returns the result in a suitable format for \code{\link{mcmcMH}}.
#' @param margLogLike \R-function to compute the marginal log-likelihood of \code{theta}.
#' @param ... further arguments to be passed to \code{margLogLike}
#' @inheritParams testFitmodel
#' @export
#' @seealso \code{\link{trajLogLike}}, \code{\link{margLogLikeSto}}
#' @return a list of two elements
#' \itemize{
#' 	\item \code{log.density} numeric, logged value of the posterior density evaluated at \code{theta}
#' 	\item \code{trace} named vector with trace information (theta, log.prior, marg.log.like, log.posterior)
#' }
logPosterior <- function(fitmodel, theta, state.init, data, margLogLike = trajLogLike, ...) {

	theta.log.prior <- fitmodel$logPrior(theta=theta)

	if(is.finite(theta.log.prior)){
		theta.log.like <- margLogLike(fitmodel=fitmodel, theta=theta, state.init=state.init, data=data, ...)
	}else{
		# do not compute log-likelihood (theta prior is 0)
		theta.log.like  <-  -Inf
	}

	theta.log.posterior <- theta.log.prior + theta.log.like

	return(list(log.density=theta.log.posterior, trace=c(theta,log.prior=theta.log.prior,marg.log.like=theta.log.like,log.posterior=theta.log.posterior)))

}

#' Generate an observation trajectory for a fitmodel
#'
#' This function simulates a model defined in a \code{\link{fitmodel}}
#' object and generates observations at each time point. It returns
#' the trajectory with an additions \code{obs} column.
#' @inheritParams testFitmodel
#' @param times the times at which to generate observations
#' @export
#' @seealso \code{\link{trajLogLike}}
#' @return numeric value of the log-likelihood
genObsTraj <- function(fitmodel, theta, state.init, times) {

        ## simulate model at successive observation times of data
        traj <- fitmodel$simulate(theta, state.init, times)

        ## generate observations by applying fitmodel$genObsPoint to
        ## each row of traj. The parameter value theta as passed as
        ## extra argument to fitmodel$genObsPoint
        traj$obs <- apply(X = traj, MARGIN = 1, FUN = fitmodel$genObsPoint,
                          theta = theta)
        return(traj)
}
