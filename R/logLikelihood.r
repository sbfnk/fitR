#' Log-likelihood of a trajectory for a deterministic model
#'
#' Compute the trajectory log-likelihood of \code{theta} for a
#' deterministic model defined in a \code{\link{fitmodel}} object by
#' summing the point log-likelihoods.
#' @inheritParams testFitmodel
#' @export
#' @import deSolve
#' @seealso \code{\link{rTrajObs}}
#' @return numeric value of the log-likelihood
dTrajObs <- function(fitmodel, theta, init.state, data, log = FALSE) {

	# time sequence (must include initial time)
	times <- c(0,data$time)

	# simulate model at successive observation times of data
	traj <- fitmodel$simulate(theta,init.state,times)

	dens <- 0

        # compute log-likelihood by summing the log-likelihood of each data point
	for(i in 1:nrow(data)){

		# extract data point
		data.point <- unlist(data[i,])

		# extract model point
		# we use i+1 since the first row of traj contains the initial state.
		model.point <- unlist(traj[i+1,])

		# update marginal log-likelihood
                dens <- dens + fitmodel$dPointObs(data.point=data.point, model.point=model.point, theta=theta, log = TRUE)
	}

	return(ifelse(log, dens, exp(dens))
}

#'Marginal log-likelihood for a stochastic model
#'
#'Compute a Monte-Carlo estimate of the log-likelihood of \code{theta} for a stochastic model defined in a \code{\link{fitmodel}} object, using \code{\link{particleFilter}}
#' @inheritParams testFitmodel
#' @inheritParams particleFilter
#' @export
#' @seealso particleFilter
#' @return Monte-Carlo estimate of the marginal log-likelihood of \code{theta}
margLogLikeSto <- function(fitmodel, theta, init.state, data, n.particles, n.cores = 1) {

	# run SMC
	smc <- particleFilter(fitmodel=fitmodel, theta=theta, init.state=init.state, data=data, n.particles=n.particles, n.cores=n.cores)

	return(smc$margLogLike)
}

#'Posterior distribution for a fitmodel
#'
#'This function evaluates the posterior distribution at \code{theta} and returns the result in a suitable format for \code{\link{mcmcMH}}.
#' @param margLogLike \R-function to compute the marginal log-likelihood of \code{theta}.
#' @param ... further arguments to be passed to \code{margLogLike}
#' @inheritParams testFitmodel
#' @export
#' @seealso \code{\link{dTrajObs}}, \code{\link{margLogLikeSto}}
#' @return a list of two elements
#' \itemize{
#' 	\item \code{log.density} numeric, logged value of the posterior density evaluated at \code{theta}
#' 	\item \code{trace} named vector with trace information (theta, log.prior, marg.log.like, log.posterior)
#' }
logPosterior <- function(fitmodel, theta, init.state, data, margLogLike = dTrajObs, ...) {

	log.prior <- fitmodel$dprior(theta=theta)

	if(is.finite(log.prior)){
		log.likelihood <- margLogLike(fitmodel=fitmodel, theta=theta, init.state=init.state, data=data, ...)
	}else{
		# do not compute log-likelihood (theta prior is 0)
		log.likelihood  <-  -Inf
	}

	log.posterior <- log.prior + log.likelihood

	return(list(log.density=log.posterior, trace=c(theta,log.prior=log.prior,log.likelihood=log.likelihood,log.posterior=log.posterior)))

}


#'A wrapper for \code{logPosterior}
#'
#'A wrapper for \code{\link{logPosterior}} that returns a function that can be used as a \code{target} for \code{\link{mcmcMH}}
#' @inheritParams logPosterior
#' @export
#' @return a \R-function with one argument called \code{theta}.
logPosteriorWrapper <- function(fitmodel, init.state, data, margLogLike, ...) {

	function(theta) {
		logPosterior(fitmodel, theta, init.state, data, margLogLike, ...)	
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
#' @return data.frame
rTrajObs <- function(fitmodel, theta, init.state, times) {

        ## simulate model at successive observation times of data
	traj <- fitmodel$simulate(theta, init.state, times)

        ## generate observations by applying fitmodel$rPointObs to
        ## each row of traj. The parameter value theta as passed as
        ## extra argument to fitmodel$rPointObs
	
	obs <- ddply(traj, "time" , fitmodel$rPointObs, theta = theta)
	traj_obs <- join(traj,obs, by="time")

	return(traj_obs)


}



#'Compute the DIC
#'
#'This function computes the Deviance Information Criterion (DIC) of a \code{\link{fitmodel}} from a MCMC sample.
#' @param trace either a \code{data.frame} or \code{mcmc} object. Must contain one column with the posterior \code{log.likelihood}.
#' @inheritParams testFitmodel
#' @inheritParams logPosterior
#' @export
#' @return a list of 5 elements:
#' \itemize{
#'     \item \code{DIC} value of the DIC
#'     \item \code{theta_bar} mean posterior of theta
#'     \item \code{log_like_theta_bar} log-likelihood of \code{theta_bar}
#'     \item \code{D_theta_bar} deviance of \code{theta_bar}
#'     \item \code{p_D} effective number of parameters
#' }
computeDIC <- function(trace, fitmodel, init.state, data, margLogLike = dTrajObs, ...) {

    simulation <- match.arg(simulation)

    # compute mean posterior estimate
    theta_bar <- colMeans(trace[fitmodel$theta.names])

    log_like_theta_bar <- margLogLike(fitmodel, theta_bar, init.state, data = data, ...)

    # and its deviance
    D_theta_bar <- -2 * log_like_theta_bar

    # the effective number of parameters
    p_D <- var(-2 * trace$log.likelihood)/2

    # and finally the DIC
    DIC <- D_theta_bar + 2 * p_D

    ans <- list(DIC=DIC, theta_bar=theta_bar, log_like_theta_bar=log_like_theta_bar, D_theta_bar=D_theta_bar,  p_D=p_D)

    return(ans)
}






