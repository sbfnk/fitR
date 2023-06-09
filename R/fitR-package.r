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
NULL
