#'ABC distance with oscillations
#'
#'This positive distance is the mean squared differences between the simulation and the observation, divided by the square of the number of times the simulation oscillates around the observation.
#' @param simu.traj.obs \code{data.frame} of simulated trajectory with observation, as returned by \code{\link{genObsTraj}}.
#' @param data \code{data.frame} of times and observations. Must have two columns: \code{time} and \code{Inc}.
#' @export
#' @seealso distanceOscillation
#' @examples \dontrun{
#' # Suppose we observed a time series:
#' data <- data.frame(time=1:7,Inc=c(1,3,5,7,5,3,1))
#' # and we have two simulated time series:
#' traj1 <- data.frame(time=1:7,observation=c(3,5,7,9,7,5,3))
#' traj2 <- data.frame(time=1:7,observation=c(3,5,3,5,7,5,3))
#' # traj1 is consistently above data and traj2 oscillates around data:
#' plot(data$time,data$Inc,t='l',ylim=c(0,10))
#' lines(traj1$time,traj1$observation,col="red")
#' lines(traj2$time,traj2$observation,col="blue")
#' # While the squared differences are the same, we obtain a smaller distance for traj2:
#' d1 <- SEITL_distanceOscillation(traj1,data)
#' # d1 = 4
#' d2 <- SEITL_distanceOscillation(traj2,data)
#' # d2 = 1.3
#'}
SEITL_distanceOscillation <- function(simu.traj.obs, data) {

	# match model and data on time
	keep.time <- intersect(simu.traj.obs$time,data$time)
	simu.traj.obs <- subset(simu.traj.obs,time%in%keep.time)
	data <- subset(data,time%in%keep.time)
	
	x <- simu.traj.obs$observation
	y <- data$Inc

	return(distanceOscillation(x,y))
}


#'Compute the distance to the data for ABC
#'
#'Compute the distance (using \code{distance.ABC}) between the observed time series and a simulated time series obtained by running the model with parameters \code{theta}.
#' @param distance.ABC a function that take two arguments: \code{data} and \code{simu.traj.obs}
#' @inheritParams testFitmodel
#' @export
#' @return numeric value of the log-likelihood
computeDistanceABC <- function(fitmodel, theta, init.state, data, distance.ABC) {

	# time sequence (must include initial time)
	times <- c(0,data$time)

	# generate simulated observation
	traj.obs <- genObsTraj(model = fitmodel, theta=theta, init.state=init.state, times=times)

	# compute distance
	dist.ABC <- distance.ABC(data=data,simu.traj.obs=traj.obs)

	return(dist.ABC)
}

#'Target ABC posterior distribution for a fitmodel
#'
#'This function evaluates the ABC posterior distribution at \code{theta} and returns the result in a suitable format for \code{\link{mcmcMH}}.
#' @param epsilon numeric vector, ABC tolerances for distances between data and simulations.
#' @inheritParams testFitmodel
#' @export
#' @seealso computeDistanceABC
#' @return a list of two elements
#' \itemize{
#' 	\item \code{log.dist} numeric, logged value of the ABC posterior distribution evaluated at \code{theta}
#' 	\item \code{trace} named vector with trace information (theta, logPrior, distance.ABC, log.posterior)
#' }
targetPosteriorABC <- function(theta,fitmodel,epsilon) {

	theta.logPrior <- fitmodel$logPrior(theta=theta)

	if(is.finite(theta.logPrior)){
		theta.dist.ABC <- computeDistanceABC(theta,fitmodel)
		if(length(epsilon)!=length(theta.dist.ABC)){
			stop("Length of ",sQuote("epsilon")," differs from the length of the distance vector returned by ",sQuote("fitmodel$distance.ABC"),call.=FALSE)
		}
		theta.log.posterior <- theta.logPrior + log(ifelse(all(theta.dist.ABC <= epsilon),1,0))

	}else{
		# do not compute ABC distance (theta prior is 0)
		theta.dist.ABC  <- Inf
		theta.log.posterior  <-  -Inf
	}


	return(list(log.dist=theta.log.posterior, trace=c(theta,logPrior=theta.logPrior,distance.ABC=theta.dist.ABC,log.posterior=theta.log.posterior)))

}
