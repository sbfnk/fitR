#' Time-series of the 1971 influenza epidemic in Tristan-da-Cunha
#'
#' A dataset containing the daily incidence recorded during the 1971 influenza
#' A/H3N2 two-wave epidemic on the island of Tristan-da-Cunha.
#'
#' \itemize{
#'   \item \code{date} calendar date of the record
#'   \item \code{time} day of record since beginning of epidemic
#'   \item \code{Inc} daily count incidence of influenza-like-illness
#' }
#'
#' @format A data frame with 59 rows and 3 variables
#' @source \url{http://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&id=4511951&retmode=ref&cmd=prlinks} # nolint
#' @name fluTdc1971
#' @keywords internal
NULL

#' A simple deterministic SIR model with constant population size
#'
#' A simple deterministic SIR model with constant population size, uniform prior
#' and Poisson observation.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{stateNames} character vector.
#'   \item \code{thetaNames} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#'
#' Look at the documentation of \code{\link{fitmodel}} for more details about
#' each of these elements.
#' You can look at the code of the \R-functions by typing
#' \code{sirDeter$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following
#'   elements:
#' @name sirDeter
#' @keywords internal
NULL

#' A simple stochastic SIR model with constant population size
#'
#' A simple stochastic SIR model with constant population size, uniform prior
#' and Poisson observation.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{stateNames} character vector.
#'   \item \code{thetaNames} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#'
#' Look at the documentation of \code{\link{fitmodel}} for more details about
#' each of these elements.
#' You can look at the code of the \R-functions by typing
#' \code{sirStoch$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following
#'   elements:
#' @name sirStoch
#' @keywords internal
NULL

#' A simple deterministic SIR model with constant population size and reporting
#' rate
#'
#' A simple deterministic SIR model with constant population size, uniform prior
#' and Poisson observation with reporting rate.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{stateNames} character vector.
#'   \item \code{thetaNames} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#'
#' Look at the documentation of \code{\link{fitmodel}} for more details about
#' each of these elements.
#' You can look at the code of the \R-functions by typing
#' \code{sirReportingDeter$simulate} for instance. There are some comments
#' included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following
#'   elements:
#' @name sirReportingDeter
#' @keywords internal
NULL

#' A simple deterministic SIR model with constant population size and parameters
#' on the exponential scale
#'
#' A simple deterministic SIR model with constant population size, uniform prior
#' and Poisson observation. The parameters are transformed using an exponential
#' transformation.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{stateNames} character vector.
#'   \item \code{thetaNames} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#'
#' Look at the documentation of \code{\link{fitmodel}} for more details about
#' each of these elements.
#' You can look at the code of the \R-functions by typing
#' \code{sirExpDeter$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following
#'   elements:
#' @name sirExpDeter
#' @keywords internal
NULL

#' The deterministic SIR model with two populations.
#'
#' The deterministic SIR model with two populations (young/old) and constant
#' population size, uniform prior and Poisson observation.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{stateNames} character vector.
#'   \item \code{thetaNames} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#'
#' Look at the documentation of \code{\link{fitmodel}} for more details about
#' each of these elements.
#' You can look at the code of the \R-functions by typing
#' \code{seitlDeter$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following
#'   elements:
#' @name sir2popDeter
#' @keywords internal
NULL


#' The deterministic SEITL model with constant population size
#'
#' The deterministic SEITL model with constant population size, uniform prior
#' and Poisson observation with reporting rate.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{stateNames} character vector.
#'   \item \code{thetaNames} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#'
#' Look at the documentation of \code{\link{fitmodel}} for more details about
#' each of these elements.
#' You can look at the code of the \R-functions by typing
#' \code{seitlDeter$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following
#'   elements:
#' @name seitlDeter
#' @keywords internal
NULL

#' The stochastic SEITL model with constant population size
#'
#' The stochastic SEITL model with constant population size, uniform prior and
#' Poisson observation with reporting rate.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{stateNames} character vector.
#'   \item \code{thetaNames} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#'
#' Look at the documentation of \code{\link{fitmodel}} for more details about
#' each of these elements.
#' You can look at the code of the \R-functions by typing
#' \code{seitlStoch$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following
#'   elements:
#' @name seitlStoch
#' @keywords internal
NULL

#' The deterministic SEIT2L model with constant population size
#'
#' The deterministic SEIT2L model with constant population size, uniform prior
#' and Poisson observation with reporting rate.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{stateNames} character vector.
#'   \item \code{thetaNames} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#'
#' Look at the documentation of \code{\link{fitmodel}} for more details about
#' each of these elements.
#' You can look at the code of the \R-functions by typing
#' \code{seit2lDeter$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following
#'   elements:
#' @name seit2lDeter
#' @keywords internal
NULL

#' The stochastic SEIT2L model with constant population size
#'
#' The stochastic SEIT2L model with constant population size, uniform prior and
#' Poisson observation with reporting rate.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{stateNames} character vector.
#'   \item \code{thetaNames} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#'
#' Look at the documentation of \code{\link{fitmodel}} for more details about
#' each of these elements.
#' You can look at the code of the \R-functions by typing
#' \code{seit2lStoch$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following
#'   elements:
#' @name seit2lStoch
#' @keywords internal
NULL

#' The deterministic SEIT4L model with constant population size
#'
#' The deterministic SEIT4L model with constant population size, uniform prior
#' and Poisson observation with reporting rate.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{stateNames} character vector.
#'   \item \code{thetaNames} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#'
#' Look at the documentation of \code{\link{fitmodel}} for more details about
#' each of these elements.
#' You can look at the code of the \R-functions by typing
#' \code{seit4lDeter$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following
#'   elements:
#' @name seit4lDeter
#' @keywords internal
NULL

#' The stochastic SEIT4L model with constant population size
#'
#' The stochastic SEIT4L model with constant population size, uniform prior and
#' Poisson observation with reporting rate.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{stateNames} character vector.
#'   \item \code{thetaNames} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#'
#' Look at the documentation of \code{\link{fitmodel}} for more details about
#' each of these elements.
#' You can look at the code of the \R-functions by typing
#' \code{seit4lStoch$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following
#'   elements:
#' @name seit4lStoch
#' @keywords internal
NULL

#' The stochastic SEIT4L model with constant population size
#'
#' The stochastic SEIT4L model with constant population size, uniform prior and
#' Poisson observation with reporting rate.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{stateNames} character vector.
#'   \item \code{thetaNames} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#'
#' Look at the documentation of \code{\link{fitmodel}} for more details about
#' each of these elements.
#' You can look at the code of the \R-functions by typing
#' \code{seit4lStoch$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following
#'   elements:
#' @name seit4lStoch
#' @keywords internal
NULL

#' Time-series of an outbreak simulated using an SIR model.
#'
#' A dataset containing the daily observed incidence of new infections for a
#' simulated outbreak from the `sirDeter` model with R_0 = 1.8 and D_inf = 2, in
#' a population of 999 susceptibles and a single infected person.
#'
#' \itemize{
#'   \item \code{time} time step
#'   \item \code{obs} number of observations
#' }
#'
#' @format A data frame with 38 rows and 2 variables
#' @name epi1
#' @keywords internal
NULL

#' Time-series of an outbreak simulated using an SIR model with underreporting.
#'
#' A dataset containing the daily observed incidence of new infections for a
#' simulated outbreak from the `sirReportingDeter` model with R_0 = 2.5, D_inf =
#' 2 and RR = 0.1, in a population of 999 susceptibles and a single infected
#' person.
#'
#' \itemize{
#'   \item \code{time} time step
#'   \item \code{obs} number of observations
#' }
#'
#' @format A data frame with 26 rows and 2 variables
#' @name epi2
#' @keywords internal
NULL

#' Time-series of an outbreak simulated using an SIR model.
#'
#' A dataset containing the daily observed incidence of new infections for a
#' simulated outbreak from the `sirDeter` model with R_0 = 2.1 and D_inf = 3, in
#' a population of 999 susceptibles and a single infected person.
#'
#' \itemize{
#'   \item \code{time} time step
#'   \item \code{obs} number of observations
#' }
#'
#' @format A data frame with 44 rows and 2 variables
#' @name epi3
#' @keywords internal
NULL

#' Time-series of an outbreak simulated using an SIR model with underreporting.
#'
#' A dataset containing the daily observed incidence of new infections for a
#' simulated outbreak from the `sirReportingDeter` model with R_0 = 2.4, D_inf =
#' 8 and RR = 0.25, in a population of 999 susceptibles and a single infected
#' person.
#'
#' \itemize{
#'   \item \code{time} time step
#'   \item \code{obs} number of observations
#' }
#'
#' @format A data frame with 100 rows and 2 variables
#' @name epi4
#' @keywords internal
NULL

#' Markov-chain Monte Carlo (MCMC) trace of a fit of the SIR model to a
#' simulated outbreak
#'
#' A dataset containing the MCMC trace of a fit of the `sirDeter` model to the
#' `epi1` data set.
#'
#' \itemize{
#'   \item \code{R_0} basic reproduction number
#'   \item \code{D_inf} duration of infection
#' }
#'
#' @format A data frame with 10001 rows and 2 variables
#' @name mcmcEpi1
#' @keywords internal
NULL

#' Markov-chain Monte Carlo (MCMC) trace of a fit of the SIR model to a
#' simulated outbreak
#'
#' A dataset containing the MCMC trace of a fit of the `sirDeter` model to the
#' `epi3` data set.
#'
#' \itemize{
#'   \item \code{R_0} basic reproduction number
#'   \item \code{D_inf} duration of infection
#' }
#'
#' @format A data frame with 10001 rows and 2 variables
#' @name mcmcEpi3
#' @keywords internal
NULL

#' Markov-chain Monte Carlo (MCMC) trace of a fit of the SIR model to a
#' simulated outbreak
#'
#' A dataset containing the MCMC trace of a fit of the `sirDeter` model to the
#' `epi4` data set.
#'
#' \itemize{
#'   \item \code{R_0} basic reproduction number
#'   \item \code{D_inf} duration of infection
#'   \item \code{RR} reporting rate
#' }
#'
#' @format A data frame with 10001 rows and 2 variables
#' @name mcmcEpi4
#' @keywords internal
NULL

#' Markov-chain Monte Carlo (MCMC) outputs of a fit of the SEITL model to the
#' influenza outbreak on Tristan da Cunha (short run)
#'
#' A dataset containing the MCMC outputs of a fit of the `seitlDeter` model to
#' the `fluTdc1971` data set (5000 iterations).
#'
#' \itemize{
#'   \item \code{trace} data frame containing the MCMC trace
#'   \item \code{acceptanceRate} a single value denoting the acceptance rate
#'   \item \code{covmatEmprical} the covariance matrix of parameter samples
#' }
#'
#' @format A list with three elements describing the MCMC outputs
#' @name mcmcSeitl
#' @keywords internal
NULL

#' Markov-chain Monte Carlo (MCMC) outputs of a fit of the SEITL model to the
#' influenza outbreak on Tristan da Cunha (long run)
#'
#' A dataset containing the MCMC outputs of a fit of the `seitlDeter` model to
#' the `fluTdc1971` data set (50000 iterations). This differs from the
#' `mcmcSeitlTheta2` dataset in the choice of initial parameter set theta.
#'
#' \itemize{
#'   \item \code{trace} data frame containing the MCMC trace
#'   \item \code{acceptanceRate} a single value denoting the acceptance rate
#'   \item \code{covmatEmprical} the covariance matrix of parameter samples
#' }
#'
#' @format A list with three elements describing the MCMC outputs
#' @name mcmcSeitlTheta1
#' @keywords internal
NULL

#' Markov-chain Monte Carlo (MCMC) outputs of a fit of the SEITL model to the
#' influenza outbreak on Tristan da Cunha (long run)
#'
#' A dataset containing the MCMC outputs of a fit of the `seitlDeter` model to
#' the `fluTdc1971` data set (50000 iterations). This differs from the
#' `mcmcSeitlTheta1` dataset in the choice of initial parameter set theta.
#'
#' \itemize{
#'   \item \code{trace} data frame containing the MCMC trace
#'   \item \code{acceptanceRate} a single value denoting the acceptance rate
#'   \item \code{covmatEmprical} the covariance matrix of parameter samples
#' }
#'
#' @format A list with three elements describing the MCMC outputs
#' @name mcmcSeitlTheta2
#' @keywords internal
NULL

#' Markov-chain Monte Carlo (MCMC) outputs of a fit of the SEITL model to the
#' influenza outbreak on Tristan da Cunha (long run)
#'
#' A dataset containing the MCMC outputs of a fit of the `seitlDeter` model to
#' the `fluTdc1971` data set with informative priors (50000 iterations). This
#' differs from the `mcmcSeitlTheta1` dataset in the choice of priors and from
#' the `mcmcSeitlInfoPriorTheta2` dataset in the choice of initial parameter set
#' theta.
#'
#' \itemize{
#'   \item \code{trace} data frame containing the MCMC trace
#'   \item \code{acceptanceRate} a single value denoting the acceptance rate
#'   \item \code{covmatEmprical} the covariance matrix of parameter samples
#' }
#'
#' @format A list with three elements describing the MCMC outputs
#' @name mcmcSeitlInfoPriorTheta1
#' @keywords internal
NULL

#' Markov-chain Monte Carlo (MCMC) outputs of a fit of the SEITL model to the
#' influenza outbreak on Tristan da Cunha (long run)
#'
#' A dataset containing the MCMC outputs of a fit of the `seitlDeter` model to
#' the `fluTdc1971` data set with informative priors (50000 iterations). This
#' differs from the `mcmcSeitlTheta2` dataset in the choice of priors and from
#' the `mcmcSeitlInfoPriorTheta1` dataset in the choice of initial parameter set
#' theta.
#'
#' \itemize{
#'   \item \code{trace} data frame containing the MCMC trace
#'   \item \code{acceptanceRate} a single value denoting the acceptance rate
#'   \item \code{covmatEmprical} the covariance matrix of parameter samples
#' }
#'
#' @format A list with three elements describing the MCMC outputs
#' @name mcmcSeitlInfoPriorTheta2
#' @keywords internal
NULL

#' Markov-chain Monte Carlo (MCMC) outputs of a fit of the stochastic SEIT4L
#' model to the influenza outbreak on Tristan da Cunha
#'
#' A dataset containing the MCMC outputs of a fit of the `seit4lStoch` model to
#' the `fluTdc1971` data set with informative priors (8 chains of 3000
#' iterations each), using a particle filter with 8 particles to estimate the
#' likelihood at each MCMC setup. This differs from the `pMcmcSeitlInfoPrior64`
#' dataset in the number of particles.
#'
#' \itemize{
#'   \item \code{trace} data frame containing the MCMC trace
#'   \item \code{acceptanceRate} a single value denoting the acceptance rate
#'   \item \code{covmatEmprical} the covariance matrix of parameter samples
#' }
#'
#' @format A list with of length 8 with each element containing three elements
#'   describing the MCMC outputs
#' @name pmcmcSeit4lInfoPrior8
#' @keywords internal
NULL

#' Markov-chain Monte Carlo (MCMC) outputs of a fit of the stochastic SEIT4L
#' model to the influenza outbreak on Tristan da Cunha
#'
#' A dataset containing the MCMC outputs of a fit of the `seit4lStoch` model to
#' the `fluTdc1971` data set with informative priors (8 chains of 3000
#' iterations each), using a particle filter with 64 particles to estimate the
#' likelihood at each MCMC setup. This differs from the `pMcmcSeitlInfoPrior8`
#' dataset in the number of particles.
#'
#' \itemize{
#'   \item \code{trace} data frame containing the MCMC trace
#'   \item \code{acceptanceRate} a single value denoting the acceptance rate
#'   \item \code{covmatEmprical} the covariance matrix of parameter samples
#' }
#'
#' @format A list with of length 8 with each element containing three elements
#'   describing the MCMC outputs
#' @name pmcmcSeit4lInfoPrior64
#' @keywords internal
NULL

#' A data set resulting from calibrating the number of particles in a
#' particleFilter to estimate the likelihood of the stochastic SEIT4L
#' model to the influenza outbreak on Tristan da Cunha
#'
#' A dataset containing the results of an attempt to calibrate the number of
#' particles to be used in estimating the likelihood when fitting the
#' `seit4lStoch` model to the `fluTdc1971` dataset. Estimates were obtained
#' using 100 replicates of the estimation procedure.
#'
#' \itemize{
#'   \item \code{nParticles} number of particles
#'   \item \code{mean} mean estimated likelihood
#'   \item \code{sd} standard deviation of the estimated likelihood
#'   \item \code{propDepleted} proportion of particles depleted
#'   \item \code{days} number of days the 100 replicated took to estimate
#' }
#'
#' @format A data frame with 11 rows and 5 columns describing the results of the
#'   calibration procedure
#' @name calibrateSmc
#' @keywords internal
NULL
