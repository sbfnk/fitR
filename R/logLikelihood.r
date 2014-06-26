#'Marginal log-likelihood for a deterministic model
#'
#'Compute the marginal log-likelihood of \code{theta} for a deterministic model defined in a \code{\link{fitmodel}} object.
#' @param theta named vector of model parameters. Names must correspond to those of \code{names(fitmodel$theta)}.
#' @param fitmodel \code{\link{fitmodel}} object.
#' @export
#' @return numeric value of the log-likelihood
marginalLogLikelihoodDeterministic <- function(theta, fitmodel) {

	data <- fitmodel$data

	# time sequence (must include initial time)
	times <- c(0,data$time)

	# simulateTraj model at successive observation times of data
	traj <- fitmodel$simulateTraj(theta,fitmodel$initialise.state(theta),times)

	# compute log-likelihood
	logLikePoint <- fitmodel$logLikePoint(data=data,theta=theta,simu.traj=traj)

	return(logLikePoint)
}

#'Marginal log-likelihood for a stochastic model
#'
#'Compute a Monte-Carlo estimate of the log-likelihood of \code{theta} for a stochastic model defined in a \code{\link{fitmodel}} object, using \code{\link{bootstrapParticleFilter}}
#' @inheritParams marginalLogLikelihoodDeterministic
#' @inheritParams bootstrapParticleFilter
#' @export
#' @seealso bootstrapParticleFilter
#' @return Monte-Carlo estimate of the marginal log-likelihood of \code{theta}
marginalLogLikelihoodStochastic <- function(theta, fitmodel, n.particles, n.cores = 1) {

	# replace parameter values
	fitmodel$theta[names(theta)] <- theta

	# run SMC
	smc <- bootstrapParticleFilter(fitmodel=fitmodel, n.particles=n.particles, n.cores=n.cores)

	return(smc$logLikePoint)
}

#'Target posterior distribution for a fitmodel
#'
#'This function evaluates the posterior distribution at \code{theta} and returns the result in a suitable format for \code{\link{mcmcMH}}.
#' @param theta named vector of estimated theta
#' @param logPrior \R-function to compute the log-prior of \code{theta}, as returned by \code{\link{fitmodel}}
#' @param logPrior.args list of arguments passed to \code{logPrior}
#' @param marginal.logLikePoint \R-function to compute the marginal log-likelihood of \code{theta}
#' @param marginal.logLikePoint.args list of arguments passed to \code{marginal.logLikePoint}
#' @export
#' @return a list of two elements
#' \itemize{
#' 	\item \code{log.dist} numeric, logged value of the posterior distribution evaluated at \code{theta}
#' 	\item \code{trace} named vector with trace information (theta, logPrior, marginal.logLikePoint, log.posterior)
#' }
targetPosterior <- function(theta, logPrior, logPrior.args=list(), marginal.logLikePoint, marginal.logLikePoint.args=list()) {

	theta.logPrior <- do.call(logPrior, c(list(theta=theta),logPrior.args))

	if(is.finite(theta.logPrior)){
		theta.marginal.logLikePoint <- do.call(marginal.logLikePoint, c(list(theta=theta), marginal.logLikePoint.args))
	}else{
		# do not compute logLikePoint	(theta prior is 0)
		theta.marginal.logLikePoint  <-  -Inf
	}

	theta.log.posterior <- theta.logPrior + theta.marginal.logLikePoint

	return(list(log.dist=theta.log.posterior, trace=c(theta,logPrior=theta.logPrior,marginal.logLikePoint=theta.marginal.logLikePoint,log.posterior=theta.log.posterior)))

}



