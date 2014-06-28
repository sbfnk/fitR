#'Marginal log-likelihood for a deterministic model
#'
#'Compute the marginal log-likelihood of \code{theta} for a deterministic model defined in a \code{\link{fitmodel}} object.
#' @inheritParams testFitmodel
#' @export
#' @return numeric value of the log-likelihood
margLogLikeDeter <- function(fitmodel, theta, state.init, data) {

	# time sequence (must include initial time)
	times <- c(0,data$time)

	# simulateTraj model at successive observation times of data
	traj <- fitmodel$simulate(theta,state.init,times)

	# compute log-likelihood by summing the log-likelihood of each data point
	margLogLike <- 0
	for(i in 1:nrow(data)){ 

		# extract data point
		data.point <- unlist(data[i,])
		
		# extract state point
		# we use i+1 since the first row of traj contains the initial state.
		state.point <- unlist(traj[i+1,fitmodel$state.names])
		
		# update marginal log-likelihood
		margLogLike <- margLogLike + fitmodel$logLikePoint(data.point=data.point, state.point=state.point, theta=theta)		
	}

	return(margLogLike)
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
#' @seealso \code{\link{margLogLikeDeter}}, \code{\link{margLogLikeSto}}
#' @return a list of two elements
#' \itemize{
#' 	\item \code{log.density} numeric, logged value of the posterior density evaluated at \code{theta}
#' 	\item \code{trace} named vector with trace information (theta, log.prior, marg.log.like, log.posterior)
#' }
posteriorDensity <- function(fitmodel, theta, state.init, data, margLogLike, ...) {

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



