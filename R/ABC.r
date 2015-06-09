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


#'Compute the distance between a model and data for ABC
#'
#'Compute the distance (using \code{distance.ABC}) between the observed time series and a simulated time series of observations obtained by running the model with parameters \code{theta}.
#' @param sum.stats a list of functions to calculate summary statistics. Each of these takes one argument (a trajectory with an "obs" column) and returns a number (the summary statistic given the trajectory)
#' @param distanceABC a function that take three arguments: \code{sum.stats}, a list of summary statistics, \code{data.obs} (the data trajectory of observations) and \code{model.obs} (a model trajectory of observations), and returns the distance between the model run and the data in terms of the summary statistics
#' @inheritParams testFitmodel
#' @export
#' @return a sampled distance between
computeDistanceABC <- function(sum.stats, distanceABC, fitmodel, theta, init.state, data) {

    # time sequence (must include initial time)
    times <- c(0,data$time)

    # generate simulated observation
    model.obs <- genObsTraj(fitmodel = fitmodel, theta=theta, init.state=init.state, times=times)

    # compute distance
    dist.ABC <- distanceABC(sum.stats = sum.stats,
                             data.obs = data,
                             model.obs = model.obs)

    return(dist.ABC)
}

#'ABC logged posterior distribution
#'
#'This function evaluates the ABC posterior distribution at \code{theta} (using a single simulation trajectory) and returns the result in a suitable format for \code{\link{rmcmcMH}}.
#' @param epsilon numeric vector, ABC tolerances for distances between data and simulations. If a vector of length 1 and the distance function returns a vector of distances, this will be expanded to be same tolerance for all the parameters.
#' @inheritParams computeDistanceABC
#' @export
#' @seealso computeDistanceABC
#' @return a list of two elements
#' \itemize{
#'      \item \code{log.density} numeric, logged value of the ABC posterior distribution evaluated at \code{theta}
#'      \item \code{trace} named vector with trace information (theta, distance, log.density)
#' }
ABCLogPosterior <- function(epsilon, sum.stats, distanceABC, fitmodel, theta, init.state, data) {

    distance <-
        computeDistanceABC(sum.stats, distanceABC, fitmodel, theta, init.state, data)

    if (all(distance < epsilon)) {
        log.density <- fitmodel$logPrior(theta)
    } else {
        log.density <- -Inf
    }

    return(list(log.density = log.density,
                trace = c(theta, distance = distance, log.density = log.density)))
}

