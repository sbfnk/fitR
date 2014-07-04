#'Constructor of fitmodel object
#'
#'A \code{fitmodel} object is a \code{list} that stores some variables and functions that will be useful to simulate and fit your model during the course. 
#' @param name character. Name of the model (required).
#' @param state.names character vector. Names of the state variables i.e. \code{c("S","I","R")} (required).
#' @param theta.names character vector. Names of the parameters i.e. \code{c("R0","infectious.period")} (required).
#' @param simulate \R-function to simulate forward the model (required). This function takes 3 arguments:
#' \itemize{
#' \item \code{theta} named numeric vector. Values of the parameters. Names should match \code{theta.names}. 
#' \item \code{init.state} named numeric vector. Initial values of the state variables. Names should match \code{state.names}. 
#' \item \code{times} numeric vector. Time sequence for which the state of the model is wanted; the first value of times must be the initial time, i.e. the time of \code{init.state}.
#' }
#' and returns a \code{data.fame} containing the simulated trajectories that is the values of the state variables (1 per column) at each observation time (1 per row). The first column is \code{time}.
#' @param genObsPoint \R-function that generates a (randomly sampled) observation point from a model point, using an observation model (optional). It thus acts as an inverse of \code{pointLogLike} (see below). This function takes 2 arguments
#' \itemize{
#' \item \code{model.point} named numeric vector. State of the model at a given point in time.
#' \item \code{theta} named numeric vector. Values of the parameters. Names should match \code{theta.names}. 
#' }
#' and returns an observation point
#' @param logPrior \R-function that evaluates the log-prior density of the parameters at a given \code{theta} (optional). The function should take 1 argument:
#'\itemize{
#' 	\item \code{theta} named numeric vector. Values of the parameters. Names should match \code{theta.names}. 
#' }
#' and returns the logged value of the prior density distribution.
#' @param pointLogLike \R-function that evaluates the log-likelihood of one data point given the state of the model at the same time point. This function takes 3 arguments:
#' \itemize{
#' \item \code{data.point} named numeric vector. Observation time and observed data point.
#' \item \code{model.point} named numeric vector containing the state of the model at the observation time point.
#' \item \code{theta} named numeric vector. Parameter values. Useful since parameters are usually needed to compute the likelihood (i.e. reporting rate).
#' }
#' and returns the log-likelihood. (optional)
#' @export
#' @return a \code{fitmodel} object that is a \code{list} of 7 elements:
#' \itemize{
#' 	\item \code{name} character, name of the model
#' 	\item \code{state.names} vector, names of the state variables.
#' 	\item \code{theta.names} vector, names of the parameters.
#' 	\item \code{simulate} \R-function to simulate forward the model; usage: \code{simulate(theta,init.state,times)}.
#' 	\item \code{genObsPoint} \R-function to generate simulated observations; usage: \code{genObsPoint(model.point, theta)}.
#' 	\item \code{logPrior} \R-function to evaluate the log-prior of the parameter values; usage: \code{logPrior(theta)}.
#' 	\item \code{pointLogLike} \R-function to evaluate the log-likelihood of one data point; usage: \code{pointLogLike(data.point, model.point, theta)}.
#' }
#' @seealso \code{\link{testFitmodel}}
#' @example inst/examples/example-fitmodel.r
fitmodel <- function(name=NULL, state.names=NULL, theta.names=NULL, simulate=NULL, genObsPoint=NULL, logPrior=NULL, pointLogLike=NULL){

	# mandatory
	if(!is.character(name)){
		stop(sQuote("name")," argument is not a character")
	}
	if(!is.character(state.names)){
		stop(sQuote("state.names")," argument is not a character vector")
	}
	if(!is.character(theta.names)){
		stop(sQuote("theta.names")," argument is not a character vector")
	}
	if(!is.function(simulate)){
		stop(sQuote("simulate")," argument is not an R function")
	}
	
	# optional
	if(!is.null(genObsPoint) && !is.function(genObsPoint)){
		stop(sQuote("genObsPoint")," argument is not an R function")
	}
	if(!is.null(logPrior) && !is.function(logPrior)){
		stop(sQuote("logPrior")," argument is not an R function")
	}
	if(!is.null(pointLogLike) && !is.function(pointLogLike)){
		stop(sQuote("pointLogLike") ," argument is not an R function")
	}

	# create and return object
	return(structure(list(
		name=name,
		state.names=state.names,
		theta.names=theta.names,
		simulate=simulate,
		genObsPoint=genObsPoint,
		logPrior=logPrior,
		pointLogLike=pointLogLike), class="fitmodel"))

}

#'Test a fitmodel
#'
#'This function performs a serie of checks on the \code{\link{fitmodel}} provided by the user in order to make sure that it will be compatible both with the functions coded during the course and the functions
#'available in the \code{fitR} package. The latters can be used as a correction.
#' @param fitmodel a \code{\link{fitmodel}} object
#' @param theta named numeric vector. Values of the parameters. Names should match \code{fitmodel$theta.names}.
#' @param init.state named numeric vector. Initial values of the state variables. Names should match \code{fitmodel$state.names}.
#' @param data data frame. Observation times and observed data. The time column must be named \code{"time"} and the observation column must be named \code{"obs"}.
#' @param verbose if \code{TRUE}, print details of the test performed to check validity of the arguments
#' @export
#' @seealso \code{\link{fitmodel}}
#' @example inst/examples/example-fitmodel.r
testFitmodel <- function(fitmodel, theta, init.state, data = NULL, verbose=TRUE) {

        if (missing(fitmodel)) { stop(sQuote("fitmodel"), " argument missing\n") }
	if(!inherits(fitmodel,"fitmodel")){
		stop(sQuote("fitmodel")," argument is not from the class fitmodel")
	}

    ## test of theta
        if (missing(theta)) { stop(sQuote("theta"), " argument missing\n") }
	if(verbose){
		cat("--- checking ", sQuote("theta"), "argument\n")
		cat("Should contain the parameters:",sQuote(fitmodel$theta.names),"\nTest:\n")
		print(theta)
	}
	if(length(x <- setdiff(fitmodel$theta.names, names(theta)))){
		stop("The following parameters are missing in argument ",sQuote("theta"),": ",sQuote(x), call.=FALSE)
	}
	if(length(x <- setdiff(names(theta), fitmodel$theta.names))){
		stop("The following parameters are not in ",sQuote("fitmodel$theta.names"),": ",sQuote(x), call.=FALSE)
	}	
	if(verbose){
		cat("--> ",sQuote("theta")," argument looks good!\n")
	}

	## test of init.state
        if (missing(init.state)) { stop(sQuote("init.state"), " argument missing\n") }
	if(verbose){
		cat("--- checking ", sQuote("init.state"), "argument\n")
		cat("Should contain the states:",sQuote(fitmodel$state.names),"\nTest:\n")
		print(init.state)
	}
	if(length(x <- setdiff(fitmodel$state.names, names(init.state)))){
		stop("The following states are missing in argument ",sQuote("init.state"),": ",sQuote(x), call.=FALSE)
	}
	if(length(x <- setdiff(names(init.state), fitmodel$state.names))){
		stop("The following states are not in ",sQuote("fitmodel$state.names"),": ",sQuote(x), call.=FALSE)
	}	
	if(verbose){
		cat("--> ",sQuote("init.state")," argument looks good!\n")
	}

	test.traj <- NULL

	## check simulate
	if(!is.null(fitmodel$simulate)) {
		if(verbose){
			cat("--- checking simulate\n")
		}
                ## check arguments
		fun_args <- c("theta","init.state","times")
		if(!(all(x <- fun_args%in%names(formals(fitmodel$simulate))))){
			stop("argument(s) ",sQuote(fun_args[!x])," missing in function simulate, see ?fitmodel.")
		}

		if (!is.null(init.state)) {
			times <- 0:10
			test.traj <- fitmodel$simulate(theta=theta,init.state=init.state,times=times)
			# must return a data.frame of dimension 11x(length(state.names)+1)
			if(verbose){
				cat("simulate(theta, init.state, times=0:10) should return a non-negative data.frame of dimension",length(times),"x",length(fitmodel$state.names)+1,"with column names:",sQuote(c("time",fitmodel$state.names)),"\nTest:\n")
				print(test.traj)
			}
			if(!is.data.frame(test.traj)){
				stop("simulate must return a data.frame")
			}
			if(!all(x <- c("time",fitmodel$state.names)%in%names(test.traj))){
				stop("Column(s) missing in the data.frame returned by simulate: ",sQuote(c("time",fitmodel$state.names)[!x]))
			}
			if(!all(x <- names(test.traj)%in%c("time",fitmodel$state.names))){
				warning("The following columns are not required in the data.frame returned by simulate: ",sQuote(names(test.traj)[!x]))
			}
			if(any(test.traj$time!=times)){
				stop("The time column of the data.frame returned by simulate is different from its times argument",call.=FALSE)
			}
			if(any(test.traj<0)){
				stop("simulate returned negative values during the test, use verbose argument of fitmodel to check")
			}
			if(verbose){
				cat("--> simulate looks good!\n")
			}
		} else {
			warning("init.state not given, not creating test trajectory\n")
		}
	} else {
		warning("fitmodel does not contain a simulate method -- not tested\n")
	}

	## check genObsPoint
	if(!is.null(fitmodel$genObsPoint)) {
		if(verbose){
			cat("--- checking genObsPoint\n")
		}
                ## check arguments
		fun_args <- c("model.point","theta")
		if(!(all(x <- fun_args%in%names(formals(fitmodel$genObsPoint))))){
			stop("argument(s) ",sQuote(fun_args[!x])," missing in function genObsPoint, see ?fitmodel.")
		}

		if (!is.null(test.traj)) {
			test.genObsPoint <- fitmodel$genObsPoint(unlist(test.traj[1, ]), theta)
			if(verbose){
				cat("genObsPoint(test.traj, theta) should return a number\nTest:\n")
				print(test.genObsPoint)
			}
			if(!is.numeric(test.genObsPoint)){
				stop("genObsPoint must return a number")
			}
			if(test.genObsPoint<0){
				stop("genObsPoint returned negative observation during the test, use verbose argument of fitmodel to check")
			}
			if(verbose){
				cat("--> genObsPoint looks good!\n")
			}
		} else {
			warning("no test trajectory created, not creating test observation\n")
		}
	}


	if (!is.null(fitmodel$logPrior)) {
		if(verbose){
			cat("--- checking logPrior\n")
		}

		# check arguments
		fun_args <- c("theta")
		if(!(all(x <- fun_args%in%names(formals(fitmodel$logPrior))))){
			stop("arguments ",sQuote(fun_args[!x])," missing in function logPrior, see ?fitmodel.")
		}

		# test it
		test.logPrior <- fitmodel$logPrior(theta)
		if(verbose){
			cat("logPrior(theta) should return a single finite value\nTest:",test.logPrior,"\n")
		}
		if(!(!is.na(test.logPrior) && (is.finite(test.logPrior)))){
			stop("logPrior must return a finite value for test parameter values")
		}
		if(verbose){
			cat("--> logPrior looks good!\n")
		}
	} else {
		warning("fitmodel does not contain a logPrior method -- not tested\n")
	}

	# data must have a column named time, should not start at 0
	if (!is.null(data)) {
		if(!all(c("time","obs")%in%names(data))){
			stop(sQuote("data")," argument must have columns named ",sQuote("time")," and ", sQuote("obs"))
		}else if(data$time[1]==0){
			stop("the first observation time in data argument should not be 0")
		}
	}

    ## check pointLogLike
    ## check arguments, return value
	if (!is.null(fitmodel$pointLogLike)) {

		if(verbose){
			cat("--- checking pointLogLike\n")
		}
		# check arguments
		fun_args <- c("data.point","model.point","theta")
		if(!(all(x <- fun_args%in%names(formals(fitmodel$pointLogLike))))){
			stop("argument(s) ",sQuote(fun_args[!x])," missing in function pointLogLike, see documentation.")
		}

		if (!is.null(data)) {
			if (!is.null(test.traj)) {

                ## test it, first data point corresponds to second simulation step (first row contain initial state)
				data.point <- unlist(data[1,])
				model.point <- unlist(test.traj[2,])
				test.pointLogLike <- fitmodel$pointLogLike(data.point=data.point, model.point=model.point ,theta=theta)

				if(verbose){
					cat("pointLogLike(data.point,model.point,theta) should return a single value\nTest:",test.pointLogLike,"\n")
				}
				if(length(test.pointLogLike) > 1 || is.na(test.pointLogLike) || (test.pointLogLike > 0)){
					stop("pointLogLike must return a single non-positive value")
				}
				if(verbose){
					cat("--> pointLogLike looks good!\n")
				}
			} else {
				warning("no test trajectory created, not creating test observation\n")
			}
		} else {
			warning("data argument not given -- not testing pointLogLike function")
		}
	} else {
		warning("fitmodel does not contain a pointLogLike method -- not tested\n")
	}

}

#'The SIR model
#'
#' To get the SIR model, type \code{example(SIR)}. This executes the
#' code listed below under "Examples".
#' @usage example(SIR)
#' @seealso \code{\link{fitmodel}}
#' @example inst/examples/example-fitmodel.r
SIR <- function() {
        cat ("To load the SIR model, type example(SIR)\n")
}

#'The stochastic SIR model
#'
#' To get the stochastic SIR model, type
#' \code{example(SIR_stochastic)}. This executes the code listed below
#' under "Examples".
#' @usage example(SIR_stochastic)
#' @seealso \code{\link{fitmodel}}
#' @example inst/examples/example-fitmodel-stochastic.r
SIR_stochastic <- function() {
        cat ("To load the stochastic SIR model, type example(SIR_stochastic)\n")
}

#'The SIR model with incomplete reporting
#'
#' To get the SIR model with incomplete reporting, type
#' \code{example(SIR_reporting)}. This executes the code listed below
#' under "Examples".
#' @usage example(SIR_reporting)
#' @seealso \code{\link{fitmodel}}
#' @example inst/examples/example-fitmodel-reporting.r
SIR_reporting <- function() {
        cat ("To load the SIR model with incomplete reporting, type example(SIR_reporting)\n")
}


#'The deterministic SEITL model
#'
#' To get the deterministic SEITL model, type
#' \code{example(SEITL_deter)}. This executes the code listed below
#' under "Examples".
#' @usage example(SEITL_deter)
#' @seealso \code{\link{fitmodel}}
#' @example inst/examples/example-SEITL-deter.r
SEITL_deter <- function() {
        cat ("To load the deterministic SEITL model, type example(SEITL_deter)\n")
}


#'The stochastic SEITL model
#'
#' To get the stochastic SEITL model, type
#' \code{example(SEITL_sto)}. This executes the code listed below
#' under "Examples".
#' @usage example(SEITL_sto)
#' @seealso \code{\link{fitmodel}}
#' @example inst/examples/example-SEITL-sto.r
SEITL_sto <- function() {
        cat ("To load the stochastic SEITL model, type example(SEITL_sto)\n")
}


#'The deterministic SEIT2L model
#'
#' To get the deterministic SEIT2L model, type
#' \code{example(SEIT2L_deter)}. This executes the code listed below
#' under "Examples".
#' @usage example(SEIT2L_deter)
#' @seealso \code{\link{fitmodel}}
#' @example inst/examples/example-SEIT2L-deter.r
SEIT2L_deter <- function() {
        cat ("To load the deterministic SEIT2L model, type example(SEIT2L_deter)\n")
}


#'The stochastic SEIT2L model
#'
#' To get the stochastic SEIT2L model, type
#' \code{example(SEIT2L_sto)}. This executes the code listed below
#' under "Examples".
#' @usage example(SEIT2L_sto)
#' @seealso \code{\link{fitmodel}}
#' @example inst/examples/example-SEIT2L-sto.r
SEIT2L_sto <- function() {
        cat ("To load the stochastic SEIT2L model, type example(SEIT2L_sto)\n")
}


#'Three sets of guess values for the SEITL model
#'
#' To get 3 example of parameters and initial state values, type
#' \code{example(SEITL_guess_values)}. This executes the code listed below
#' under "Examples".
#' @usage example(SEITL_guess_values)
#' @example inst/examples/example-SEITL-guess-values.r
SEITL_guess_values <- function() {
        cat ("To load the guess values for the SEITL model, type example(SEITL_guess_values)\n")
}





