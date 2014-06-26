#'Constructor of fitmodel object
#'
#'A \code{fitmodel} object is a \code{list} that stores some variables and functions that will be useful to simulate and fit your model during the course. 
#' @param name character. Name of the model (required).
#' @param state.names character vector. Names of the state variables i.e. \code{c("S","I","R")} (required).
#' @param theta.names character vector. Names of the parameters i.e. \code{c("R0","infectious.period")} (required).
#' @param simulateTraj \R-function to simulate forward the model (required). This function takes 3 arguments:
#' \itemize{
#' \item \code{theta} named numeric vector. Parameter values.
#' \item \code{state.init} named numeric vector. Initial state of the model.
#' \item \code{times} numeric vector. Time sequence for which state of the model is wanted; the first value of times must be the initial time, i.e. the time of \code{state.init}.
#' }
#' and returns a \code{data.fame} containing the simulated trajectories that is the values of the state variables (1 per column) at each observation time (1 per row). The first column is \code{time}.
#' @param generateObservation \R-function that generates simulated data from a simulated trajectory using an observation model (optional). This function takes 2 arguments:
#' \itemize{
#' \item \code{simu.traj} data.frame of simulated trajectories, as returned by \code{simulateTraj}.
#' \item \code{theta} named numeric vector. Parameter values.
#' }
#' and returns the \code{simu.traj} data.frame with an additional \code{observation} column. 
#' @param logPrior \R-function that evaluates the log-prior density of the parameters at a given \code{theta} (optional). The function should take 1 argument:
#'\itemize{
#' 	\item \code{theta} named numeric vector. Parameter values.
#' }
#' and returns the logged value of the prior density distribution.
#' @param logLikePoint \R-function that evaluates the log-likelihood of one data point given the state of the model at the same time point. This function takes 3 arguments:
#' \itemize{
#' \item \code{data.point} named vector containing the observation time and the value of the data point.
#' \item \code{state.point} named vector containing the state of the model at the observation time point.
#' \item \code{theta} named numeric vector. Parameter values. Useful since parameters are usually needed to compute the likelihood (i.e. reporting rate).
#' }
#' and returns the log-likelihood. (optional)
#' @export
#' @return a \code{fitmodel} object that is a \code{list} of 9 elements:
#' \itemize{
#' 	\item \code{name} character, name of the model
#' 	\item \code{state.names} vector, names of the state variables.
#' 	\item \code{theta.names} vector, names of the parameters.
#' 	\item \code{simulateTraj} \R-function to simulateTraj forward the model; usage: \code{simulateTraj(theta,state.init,times)}.
#' 	\item \code{generateObservation} \R-function to generate simulated observation; usage: \code{generateObservation(simu.traj, theta)}.
#' 	\item \code{logPrior} \R-function to evaluate the log-prior of the parameter values; usage: \code{logPrior(theta)}.
#' 	\item \code{logLikePoint} \R-function to evaluate the log-likelihood of one data point; usage: \code{logLikePoint(data.point, state.point, theta)}.
#' }
#' @seealso \code{\link{testFitmodel}}
#' @example inst/examples/example-fitmodel.r
fitmodel <- function(name=NULL, state.names=NULL, theta.names=NULL, simulateTraj=NULL, generateObservation=NULL, logPrior=NULL, logLikePoint=NULL){

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
	if(!is.function(simulateTraj)){
		stop(sQuote("simulateTraj")," argument is not an R function")
	}
	
	# optional
	if(!is.null(generateObservation) && !is.function(generateObservation)){
		stop(sQuote("generateObservation")," argument is not an R function")
	}
	if(!is.null(logPrior) && !is.function(logPrior)){
		stop(sQuote("logPrior")," argument is not an R function")
	}
	if(!is.null(logLikePoint) && !is.function(logLikePoint)){
		stop(sQuote("logLikePoint") ," argument is not an R function")
	}

	# create and return object
	return(structure(list(
		name=name,
		state.names=state.names,
		theta.names=theta.names,
		simulateTraj=simulateTraj,
		generateObservation=generateObservation,
		logPrior=logPrior,
		logLikePoint=logLikePoint), class="fitmodel"))

}

#'Test a fitmodel
#'
#'This function performs a serie of checks on the \code{\link{fitmodel}} provided by the user in order to make sure that it will be compatible both with the functions coded during the course and the functions
#'available in the \code{fitR} package. The latters can be used as a correction.
#' @param model a \code{\link{fitmodel}} object
#' @param theta named vector of parameter values. Names should match \code{model$theta.names}. 
#' @param state.init named vector of initial state values. Names should match \code{model$state.names}. 
#' @param data data frame containing the observation times and observed data. The time column must be named \code{time}, whereas the data column should match the data name used in the function \code{model$logLikePoint}. 
#' @param verbose if \code{TRUE}, print details of the test performed to check validity of the arguments
#' @export
#' @seealso \code{\link{fitmodel}}
#' @example inst/examples/example-fitmodel.r
testFitmodel <- function(model, theta, state.init, data = NULL, verbose=TRUE) {

	if(!inherits(model,"fitmodel")){
		stop(sQuote("model")," argument is not a fitmodel")
	}

    ## test of theta
	if(verbose){
		cat("--- checking ", sQuote("theta"), "argument\n")
		cat("Should contain the parameters:",sQuote(model$theta.names),"\nTest:\n")
		print(theta)
	}
	if(length(x <- setdiff(model$theta.names, names(theta)))){
		stop("The following parameters are missing in argument ",sQuote("theta"),": ",sQuote(x), call.=FALSE)
	}
	if(length(x <- setdiff(names(theta), model$theta.names))){
		stop("The following parameters are not in ",sQuote("model$theta.names"),": ",sQuote(x), call.=FALSE)
	}	
	if(verbose){
		cat("--> ",sQuote("theta")," argument looks good!\n")
	}

	## test of state.init
	if(verbose){
		cat("--- checking ", sQuote("state.init"), "argument\n")
		cat("Should contain the states:",sQuote(model$state.names),"\nTest:\n")
		print(state.init)
	}
	if(length(x <- setdiff(model$state.names, names(state.init)))){
		stop("The following states are missing in argument ",sQuote("state.init"),": ",sQuote(x), call.=FALSE)
	}
	if(length(x <- setdiff(names(state.init), model$state.names))){
		stop("The following states are not in ",sQuote("model$state.names"),": ",sQuote(x), call.=FALSE)
	}	
	if(verbose){
		cat("--> ",sQuote("state.init")," argument looks good!\n")
	}

	test.traj <- NULL

	## check simulateTraj
	if(!is.null(model$simulateTraj)) {
		if(verbose){
			cat("--- checking simulateTraj\n")
		}
                ## check arguments
		fun_args <- c("theta","state.init","times")
		if(!(all(x <- fun_args%in%names(formals(model$simulateTraj))))){
			stop("argument(s) ",sQuote(fun_args[!x])," missing in function simulateTraj, see ?fitmodel.")
		}

		if (!is.null(state.init)) {
			times <- 0:10
			test.traj <- model$simulateTraj(theta=theta,state.init=state.init,times=times)
			# must return a data.frame of dimension 11x(length(state.names)+1)
			if(verbose){
				cat("simulateTraj(theta, state.init, times=0:10) should return a non-negative data.frame of dimension",length(times),"x",length(model$state.names)+1,"with column names:",sQuote(c("time",model$state.names)),"\nTest:\n")
				print(test.traj)
			}
			if(!is.data.frame(test.traj)){
				stop("simulateTraj must return a data.frame")
			}
			if(!all(x <- c("time",model$state.names)%in%names(test.traj))){
				stop("Column(s) missing in the data.frame returned by simulateTraj: ",sQuote(c("time",model$state.names)[!x]))
			}
			if(!all(x <- names(test.traj)%in%c("time",model$state.names))){
				warning("The following columns are not required in the data.frame returned by simulateTraj: ",sQuote(names(test.traj)[!x]))
			}
			if(any(test.traj$time!=times)){
				stop("The time column of the data.frame returned by simulateTraj is different from its times argument",call.=FALSE)
			}
			if(any(test.traj<0)){
				stop("simulateTraj returned negative values during the test, use verbose argument of fitmodel to check")
			}
			if(verbose){
				cat("--> simulateTraj looks good!\n")
			}
		} else {
			warning("state.init not given, not creating test trajectory\n")
		}
	} else {
		warning("model does not contain a simulateTraj method -- not tested\n")
	}

	## check generateObservation
	if(!is.null(model$generateObservation)) {
		if(verbose){
			cat("--- checking generateObservation\n")
		}
                ## check arguments
		fun_args <- c("simu.traj","theta")
		if(!(all(x <- fun_args%in%names(formals(model$generateObservation))))){
			stop("argument(s) ",sQuote(fun_args[!x])," missing in function generateObservation, see ?fitmodel.")
		}

		if (!is.null(test.traj)) {
			test.generateObservation <- model$generateObservation(test.traj, theta)
			if(verbose){
				cat("generateObservation(test.traj, theta) should return a non-negative data.frame of dimension",nrow(test.traj),"x",ncol(test.traj)+1,"with column names:",sQuote(c(names(test.traj),"observation")),"\nTest:\n")
				print(test.generateObservation)
			}
			if(!is.data.frame(test.generateObservation)){
				stop("generateObservation must return a data.frame")
			}
			if(!all(x <- c(names(test.generateObservation),"observation")%in%names(test.generateObservation))){
				stop("Column(s) missing in the data.frame returned by generateObservation:",sQuote(c("time",model$state.names)[x]))
			}
			if(!all(x <- names(test.generateObservation)%in%c(names(test.generateObservation),"observation"))){
				warning("The following columns are not required in the data.frame returned by generateObservation:",sQuote(names(test.generateObservation)[x]))
			}
			if(nrow(test.generateObservation)!=nrow(test.traj)){
				stop("The data.frame returned by generateObservation must have the same number of rows as the simu.traj argument",call.=FALSE)
			}
			if(any(test.generateObservation$observation<0)){
				stop("generateObservation returned negative observation during the test, use verbose argument of fitmodel to check")
			}
			if(verbose){
				cat("--> generateObservation looks good!\n")
			}
		} else {
			warning("no test trajectory created, not creating test observation\n")
		}
	}


	if (!is.null(model$logPrior)) {
		if(verbose){
			cat("--- checking logPrior\n")
		}

		# check arguments
		fun_args <- c("theta")
		if(!(all(x <- fun_args%in%names(formals(model$logPrior))))){
			stop("arguments ",sQuote(fun_args[!x])," missing in function logPrior, see ?fitmodel.")
		}

		# test it
		test.logPrior <- model$logPrior(theta)
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
		warning("model does not contain a logPrior method -- not tested\n")
	}

	# data must have a column named time, should not start at 0
	if (!is.null(data)) {
		if(!"time"%in%names(data)){
			stop(sQuote("data")," argument must have a column named ",sQuote("time"))
		}else if(data$time[1]==0){
			stop("the first observation time in data argument should not be 0")
		}
	}

    ## check logLikePoint
    ## check arguments, return value
	if (!is.null(model$logLikePoint)) {

		if(verbose){
			cat("--- checking logLikePoint\n")
		}
		# check arguments
		fun_args <- c("data.point","state.point","theta")
		if(!(all(x <- fun_args%in%names(formals(model$logLikePoint))))){
			stop("argument(s) ",sQuote(fun_args[!x])," missing in function logLikePoint, see documentation.")
		}

		if (!is.null(data)) {
			if (!is.null(test.traj)) {

                ## test it, first data point corresponds to second simulation step (first row contain initial state)
				data.point <- unlist(data[1,])
				state.point <- unlist(test.traj[2,])
				test.logLikePoint <- model$logLikePoint(data.point=data.point, state.point=state.point ,theta=theta)

				if(verbose){
					cat("logLikePoint(data.point,state.point,theta) should return a single value\nTest:",test.logLikePoint,"\n")
				}
				if(length(test.logLikePoint) > 1 || is.na(test.logLikePoint) || (test.logLikePoint > 0)){
					stop("logLikePoint must return a single non-positive value")
				}
				if(verbose){
					cat("--> logLikePoint looks good!\n")
				}
			} else {
				warning("no test trajectory created, not creating test observation\n")
			}
		} else {
			warning("data argument not given -- not testing logLikePoint function")
		}
	} else {
		warning("model does not contain a logLikePoint method -- not tested\n")
	}

}
