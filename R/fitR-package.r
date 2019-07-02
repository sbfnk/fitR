#' Time-series of the 1971 influenza epidemic in Tristan-da-Cunha
#'
#' A dataset containing the daily incidence recorded during the 1971 influenza A/H3N2 two-wave epidemic on the island of Tristan-da-Cunha.
#'
#' \itemize{
#'   \item \code{date} calendar date of the record 
#'   \item \code{time} day of record since beginning of epidemic
#'   \item \code{Inc} daily count incidence of influenza-like-illness
#' }
#'
#' @format A data frame with 59 rows and 3 variables
#' @source \url{http://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&id=4511951&retmode=ref&cmd=prlinks}
#' @name FluTdC1971
NULL

#' Time-series of a measles outbreak
#'
#' A dataset containing the weekly incidence recorded during a recent outbreak of measles in Europe
#'
#' \itemize{
#'   \item \code{time} week of the record
#'   \item \code{Inc} weekly recorded incidence of measles
#' }
#'
#' @format A data frame with 37 rows and 2 variables
#' @name measles
NULL


#' A simple deterministic SIR model with constant population size
#'
#' A simple deterministic SIR model with constant population size, uniform prior and Poisson observation.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{state.names} character vector.
#'   \item \code{theta.names} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#' 
#' Look at the documentation of \code{\link{fitmodel}} for more details about each of these elements.
#' You can look at the code of the \R-functions by typing \code{SIR$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following elements:
#' @name SIR 
NULL

#' A simple stochastic SIR model with constant population size
#'
#' A simple stochastic SIR model with constant population size, uniform prior and Poisson observation.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{state.names} character vector.
#'   \item \code{theta.names} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#' 
#' Look at the documentation of \code{\link{fitmodel}} for more details about each of these elements.
#' You can look at the code of the \R-functions by typing \code{SIR_stoch$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following elements:
#' @name SIR_stoch
NULL



#' A simple deterministic SIR model with constant population size and reporting rate
#'
#' A simple deterministic SIR model with constant population size, uniform prior and Poisson observation with reporting rate.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{state.names} character vector.
#'   \item \code{theta.names} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#' 
#' Look at the documentation of \code{\link{fitmodel}} for more details about each of these elements.
#' You can look at the code of the \R-functions by typing \code{SIR_reporting$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following elements:
#' @name SIR_reporting 
NULL



#' A simple deterministic SIR model with constant population size and parameters on the exponential scale
#'
#' A simple deterministic SIR model with constant population size, uniform prior and Poisson observation. The parameters are transformed using an exponential transformation.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{state.names} character vector.
#'   \item \code{theta.names} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#' 
#' Look at the documentation of \code{\link{fitmodel}} for more details about each of these elements.
#' You can look at the code of the \R-functions by typing \code{SIR_exp$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following elements:
#' @name SIR_exp 
NULL


#' The deterministic SEITL model with constant population size
#'
#' The deterministic SEITL model with constant population size, uniform prior and Poisson observation with reporting rate.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{state.names} character vector.
#'   \item \code{theta.names} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#' 
#' Look at the documentation of \code{\link{fitmodel}} for more details about each of these elements.
#' You can look at the code of the \R-functions by typing \code{SEITL_deter$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following elements:
#' @name SEITL_deter 
NULL


#' The stochastic SEITL model with constant population size
#'
#' The stochastic SEITL model with constant population size, uniform prior and Poisson observation with reporting rate.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{state.names} character vector.
#'   \item \code{theta.names} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#' 
#' Look at the documentation of \code{\link{fitmodel}} for more details about each of these elements.
#' You can look at the code of the \R-functions by typing \code{SEITL_stoch$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following elements:
#' @name SEITL_stoch 
NULL


#' The deterministic SEIT2L model with constant population size
#'
#' The deterministic SEIT2L model with constant population size, uniform prior and Poisson observation with reporting rate.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{state.names} character vector.
#'   \item \code{theta.names} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#' 
#' Look at the documentation of \code{\link{fitmodel}} for more details about each of these elements.
#' You can look at the code of the \R-functions by typing \code{SEIT2L_deter$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following elements:
#' @name SEIT2L_deter 
NULL


#' The stochastic SEIT2L model with constant population size
#'
#' The stochastic SEIT2L model with constant population size, uniform prior and Poisson observation with reporting rate.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{state.names} character vector.
#'   \item \code{theta.names} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#' 
#' Look at the documentation of \code{\link{fitmodel}} for more details about each of these elements.
#' You can look at the code of the \R-functions by typing \code{SEIT2L_stoch$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following elements:
#' @name SEIT2L_stoch 
NULL



#' The deterministic SEIT4L model with constant population size
#'
#' The deterministic SEIT4L model with constant population size, uniform prior and Poisson observation with reporting rate.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{state.names} character vector.
#'   \item \code{theta.names} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#' 
#' Look at the documentation of \code{\link{fitmodel}} for more details about each of these elements.
#' You can look at the code of the \R-functions by typing \code{SEIT4L_deter$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following elements:
#' @name SEIT4L_deter
NULL


#' The stochastic SEIT4L model with constant population size
#'
#' The stochastic SEIT4L model with constant population size, uniform prior and Poisson observation with reporting rate.
#'
#' \itemize{
#'   \item \code{name} character.
#'   \item \code{state.names} character vector.
#'   \item \code{theta.names} character vector.
#'   \item \code{simulate} \R-function.
#'   \item \code{rPointObs} \R-function.
#'   \item \code{dprior} \R-function.
#'   \item \code{dPointObs} \R-function.
#' }
#' 
#' Look at the documentation of \code{\link{fitmodel}} for more details about each of these elements.
#' You can look at the code of the \R-functions by typing \code{SEIT4L_stoch$simulate} for instance. There are some comments included.
#'
#' @format A \code{\link{fitmodel}} object, that is a list with the following elements:
#' @name SEIT4L_stoch 
NULL


