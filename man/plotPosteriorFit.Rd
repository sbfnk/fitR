% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.r
\name{plotPosteriorFit}
\alias{plotPosteriorFit}
\title{Plot MCMC posterior fit}
\usage{
plotPosteriorFit(
  trace,
  fitmodel,
  initState,
  data,
  posteriorSummary = c("sample", "median", "mean", "max"),
  summary = TRUE,
  sampleSize = 100,
  nonExtinct = NULL,
  alpha = min(1, 10/sampleSize),
  plot = TRUE,
  allVars = FALSE,
  initDate = NULL
)
}
\arguments{
\item{trace}{a \code{data.frame} with one column per estimated parameter, as
returned by \code{\link{burnAndThin}}}

\item{fitmodel}{a \code{\link{fitmodel}} object}

\item{initState}{named numeric vector. Initial values of the state
variables. Names should match \code{fitmodel$stateNames}.}

\item{data}{data frame. Observation times and observed data. The time column
must be named \code{"time"} and the observation column must be named
\code{"obs"}.}

\item{posteriorSummary}{character. Set to \code{"sample"} to plot
trajectories from a sample of the posterior (default). Set to
\code{"median"}, \code{"mean"} or \code{"max"} to plot trajectories
corresponding to the median, mean and maximum of the posterior density.}

\item{summary}{logical, if \code{TRUE} trajectories are summarised by their
mean, median, 50\% and 95\% quantile distributions. Otherwise, the
trajectories are plotted.}

\item{sampleSize}{number of theta sampled from posterior distribution (if
\code{posterior.summary == "sample"}). Otherwise, number of replicated
simulations.}

\item{nonExtinct}{character vector. Names of the infected states which must
be non-zero so the epidemic is still ongoing.  When the names of these
states are provided, the extinction probability is plotted by computing the
proportion of faded-out epidemics over time.  An epidemic has faded-out
when all the infected states (whose names are provided) are equal to 0.
This is only relevant for stochastic models.  In addition, if \code{summary
== TRUE}, the summaries of the trajectories conditioned on non-extinction
are shown. Default to \code{NULL}.}

\item{alpha}{transparency of the trajectories (between 0 and 1).}

\item{plot}{if \code{TRUE} the plot is displayed, and returned otherwise.}

\item{allVars}{logical, if \code{FALSE} only the observations are plotted.
Otherwise, all state variables are plotted.}

\item{initDate}{character. Date of the first point of the time series
(default to \code{NULL}). If provided, the x-axis will be in calendar
format. NB: currently only works if the unit of time is the day.}
}
\value{
If \code{plot == FALSE}, a list of 2 elements is returned:
\itemize{
   \item \code{theta} the \code{theta}(s) used for plotting (either a
  \code{vector} or a \code{data.frame})
   \item \code{traj} a \code{data.frame} with the trajectories (and
  observations) sampled from the posterior distribution.
   \item \code{plot} the plot of the fit displayed.
}
}
\description{
Plot posterior distribution of observation generated under model's posterior
parameter distribution.
}
\examples{
data(fluTdc1971)
data(epi)
data(mcmcEpi)
data(models)
initState <- c(S = 999, I = 1, R = 0)
plotPosteriorFit(
  trace = mcmcEpi1, fitmodel = sirDeter, initState = initState,
  data = epi1
 )
}
