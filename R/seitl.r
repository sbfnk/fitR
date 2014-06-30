#'Deterministic simulation of SEITL model
#'
#'Solves the system of ordinary differential equations for the SEITL model using the \code{\link[deSolve]{ode}} function.
#' @param theta named numeric vector. Parameter values. Must include the following parameters: "R0", "D.lat", "D.inf", "alpha", "D.imm" and "rho"
#' @param state.init named numeric vector. Initial state of the SEITL model. Must include the following states: "S", "E", "I", "T", "L" and "Inc".
#' @param times numeric vector. Time sequence for which state of the model is wanted; the first value of times must be the initial time, i.e. the time of \code{state.init}.
#' @export
#' @note The incidence \code{state.init[["Inc"]]} will be set to 0 at the start of the simulation and computed for each time interval of the vector \code{times}.
#' For instance, if \code{times} contains daily time steps, then the returned data frame will contain daily incidence. 
#' @import deSolve plyr
#' @return a \code{data.fame} containing the simulated trajectories that is the values of the state variables (1 per column) at each observation time (1 per row). The first column is \code{time}.
SEITL_simulateDeterministic <- function(theta,state.init,times) {

	SEITL_ode <- function(time, state, theta) {

		# param
		beta <- theta[["R0"]]/theta[["D.inf"]]
		epsilon <- 1/theta[["D.lat"]]
		nu <- 1/theta[["D.inf"]]
		alpha <- theta[["alpha"]]
		tau <- 1/theta[["D.imm"]]

		# states
		S <- state[["S"]]
		E <- state[["E"]]
		I <- state[["I"]]
		T <- state[["T"]]
		L <- state[["L"]]
		Inc <- state[["Inc"]]

		N <- S + E +I + T + L

		dS <- -beta*S*I/N + (1-alpha)*tau*T
		dE <- beta*S*I/N - epsilon*E
		dI <- epsilon*E - nu*I
		dT <- nu*I - tau*T
		dL <- alpha*tau*T
		dInc <- epsilon*E

		return(list(c(dS,dE,dI,dT,dL,dInc)))
	}


	# put incidence at 0 in state.init
	state.init["Inc"] <- 0

	traj <- as.data.frame(ode(state.init, times, SEITL_ode, theta, method = "ode45"))

	# compute incidence of each time interval
	traj <- mutate(traj,Inc=c(0,diff(Inc)))

	return(traj)

}

#'Stochastic simulation of SEITL model
#'
#'Simulate realisation of the stochastic version of the SEITL model using the \code{\link{simulateModelStochastic}} function.
#' @inheritParams SEITL_simulateDeterministic
#' @note The incidence \code{state.init[["Inc"]]} will be set to 0 at the start of the simulation and computed for each time interval of the vector \code{times}.
#' For instance, if \code{times} contains daily time steps, then the returned data frame will contain daily incidence. 
#' @import plyr
#' @export
#' @seealso \code{\link{fitmodel}}, \code{\link{simulateModelStochastic}}
#' @return a \code{data.fame} containing the simulated trajectories that is the values of the state variables (1 per column) at each observation time (1 per row). The first column is \code{time}.
SEITL_simulateStochastic <- function(theta,state.init,times) {

	
	SEITL_transitions <- list(
		c(S=-1,E=1),# infection
		c(E=-1,I=1,Inc=1),# infectiousness + incidence
		c(I=-1,T=1),# recovery + short term protection
		c(T=-1,L=1),# efficient long term protection
		c(T=-1,S=1)# deficient long term protection
		)

	SEITL_rateFunc <- function(state,theta,t) {

		# param
		beta <- theta[["R0"]]/theta[["D.inf"]]
		epsilon <- 1/theta[["D.lat"]]
		nu <- 1/theta[["D.inf"]]
		alpha <- theta[["alpha"]]
		tau <- 1/theta[["D.imm"]]

		# states
		S <- state[["S"]]
		E <- state[["E"]]
		I <- state[["I"]]
		T <- state[["T"]]
		L <- state[["L"]]
		Inc <- state[["Inc"]]

		N <- S + E +I + T + L

		return(c(
			beta*S*I/N, # infection
			epsilon*E, # infectiousness + incidence
			nu*I, # recovery + short term protection
			alpha*tau*T, # efficient long term protection
			(1-alpha)*tau*T # deficient long term protection
			)
		)
	}

	# put incidence at 0 in state.init
	state.init["Inc"] <- 0

	traj <- simulateModelStochastic(theta,state.init,times,SEITL_transitions,SEITL_rateFunc) 
	
	# compute incidence of each time interval
	traj <- mutate(traj,Inc=c(0,diff(Inc)))

	return(traj)

}

#'Generate an observed incidence
#'
#'Generate an observed incidence under a Poisson observation process.  
#' @param model.point named numeric vector. State of the model at a given point in time.
#' @param theta named numeric vector. Values of the parameters.
#' @export
#' @seealso \code{\link{SEITL_simulateDeterministic}}, \code{\link{SEITL_simulateStochastic}}
#' @return the \code{simu.traj} data.frame with an additional variable: "observation".
SEITL_genObsPoint <- function(model.point, theta){

	obs.point <- rpois(n=1, lambda=theta[["rho"]]*model.point[["Inc"]])

	return(obs.point)
}

#' Log-prior for SEITL model
#'
#' Evaluate the log of the prior density distribution of the parameter values \code{theta}.
#' @inheritParams SEITL_simulateDeterministic
#' @export
#' @seealso fitmodel, compositeLogPrior
#' @return the value of the log-prior.
SEITL_logPrior <- function(theta) {

	log.prior.R0 <- dunif(theta["R0"], min = 1, max = 100, log = TRUE)
	log.prior.latent.period <- dunif(theta["D.lat"], min = 0, max = 30, log = TRUE)
	log.prior.infectious.period <- dunif(theta["D.inf"], min = 0, max = 30, log = TRUE)
	log.prior.temporary.immune.period <- dunif(theta["D.imm"], min = 0, max = 50, log = TRUE)
	log.prior.probability.long.term.immunity <- dunif(theta["alpha"], min = 0, max = 1, log = TRUE)
	log.prior.reporting.rate <- dunif(theta["rho"], min = 0, max = 2, log = TRUE)
	
	return(log.prior.R0 + log.prior.latent.period + log.prior.infectious.period + log.prior.temporary.immune.period + log.prior.probability.long.term.immunity + log.prior.reporting.rate)

}


#'Likelihood of the data for SEITL model
#'
#'Computes the log-likelihood of a data point given the state of the model and under a poisson observation process.
#' @param data.point named vector containing the observation time and the value of the data point. Data correspond to the \code{\link{FluTdC1971}} dataset.
#' @param model.point named vector containing the state of the model at the observation time point.
#' @inheritParams SEITL_simulateDeterministic
#' @export
#' @seealso SEITL_genObsPoint
#' @return the log-likelihood value.
SEITL_pointLogLike <- function(data.point, model.point, theta){

	return(dpois(x=data.point[["obs"]],lambda=theta[["rho"]]*model.point[["Inc"]],log=TRUE))

}



#'Create the SEITL model as a fitmodel object
#'
#'This function returns a fitmodel object contaning the (either deterministic or stochastic) SEITL model, to be fitted to the \code{\link{FluTdC1971}} dataset.
#' @param simulate character. Either \code{"deterministic"} or \code{"stochastic"}.
#' @inheritParams testFitmodel
#' @export
#' @import plyr
#' @return a fitmodel object
SEITL_createFitmodel <- function(simulate=c("deterministic","stochastic")) {

	# simulate
	simulate <- match.arg(simulate)

	# simulator
	if(simulate=="deterministic"){
		simulate <- SEITL_simulateDeterministic
	} else {
		simulate <- SEITL_simulateStochastic
	}

	SEITL_name <- "SEITL model with daily incidence and constant population size"
	SEITL_state.names <- c("S","E","I","T","L","Inc")
	SEITL_theta.names <- c("R0", "D.lat", "D.inf", "alpha", "D.imm", "rho")

	# create fitmodel
	SEITL <- fitmodel(
		name=SEITL_name,
		state.names=SEITL_state.names,
		theta.names=SEITL_theta.names,
		simulate=simulate,
		genObsPoint=SEITL_genObsPoint,
		logPrior=SEITL_logPrior,
		pointLogLike=SEITL_pointLogLike)

	return(SEITL)
}
