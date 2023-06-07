#' ABC distance with oscillations
#'
#' This positive distance is the mean squared differences between the simulation
#' and the observation, divided by the square of the number of times the
#' simulation oscillates around the observation.
#' @param simuTrajObs \code{data.frame} of simulated trajectory with
#'   observation, as returned by \code{\link{rTrajObs}}.
#' @param data \code{data.frame} of times and observations. Must have two
#'   columns: \code{time} and \code{Inc}.
#' @export
#' @seealso distanceOscillation
#' @importFrom rlang .data
#' @keywords internal
#' @examples \dontrun{
#' # Suppose we observed a time series:
#' data <- data.frame(time = 1:7, Inc = c(1, 3, 5, 7, 5, 3, 1))
#' # and we have two simulated time series:
#' traj1 <- data.frame(time = 1:7, observation = c(3, 5, 7, 9, 7, 5, 3))
#' traj2 <- data.frame(time = 1:7, observation = c(3, 5, 3, 5, 7, 5, 3))
#' # traj1 is consistently above data and traj2 oscillates around data:
#' plot(data$time, data$Inc, t = "l", ylim = c(0, 10))
#' lines(traj1$time, traj1$observation, col = "red")
#' lines(traj2$time, traj2$observation, col = "blue")
#' # While the squared differences are the same, we obtain a smaller distance
#' # for traj2:
#' d1 <- SEITL_distanceOscillation(traj1, data)
#' # d1 = 4
#' d2 <- SEITL_distanceOscillation(traj2, data)
#' # d2 = 1.3
#' }
SEITL_distanceOscillation <- function(simuTrajObs, data) { # nolint
  # match model and data on time
  keepTime <- intersect(simuTrajObs$time, data$time)
  simuTrajObs <- subset(simuTrajObs, .data$time %in% keepTime)
  data <- subset(data, .data$time %in% keepTime)

  x <- simuTrajObs$observation
  y <- data$Inc

  return(distanceOscillation(x, y))
}


#' Compute the distance between a model and data for ABC
#'
#' Compute the distance (using \code{distance.ABC}) between the observed time
#' series and a simulated time series of observations obtained by running the
#' model with parameters \code{theta}.
#' @param sumStats a list of functions to calculate summary statistics. Each of
#'   these takes one argument (a trajectory with an "obs" column) and returns a
#'   number (the summary statistic given the trajectory)
#' @param distanceABC a function that take three arguments: \code{sumStats}, a
#'   list of summary statistics, \code{dataObs} (the data trajectory of
#'   observations) and \code{modelObs} (a model trajectory of observations),
#'   and returns the distance between the model run and the data in terms of the
#'   summary statistics
#' @inheritParams testFitmodel
#' @export
#' @return a sampled distance between
computeDistanceABC <- function(sumStats, distanceABC, fitmodel, theta,
                               initState, data) {
  # time sequence (must include initial time)
  times <- c(0, data$time)

  # generate simulated observation
  modelObs <- rTrajObs(
    fitmodel = fitmodel, theta = theta, initState = initState, times = times
  )

  # compute distance
  distABC <- distanceABC(
    sumStats = sumStats,
    dataObs = data,
    modelObs = modelObs
  )

  return(distABC)
}
