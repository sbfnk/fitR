#'Deterministic simulation of SEIT2L model
#'
#'Solves the system of ordinary differential equations for the SEIT2L model using the \code{\link[deSolve]{ode}} function.
#' @param theta named numeric vector. Parameter values. Must include the following parameters: "R0", "LP", "IP", "alpha", "TIP" and "rho"
#' @param state.init named numeric vector. Initial state of the SEIT2L model. Must include the following states: "S", "E", "I", "T1", "T2", "L" and "Inc".
#' @param times numeric vector. Time sequence for which state of the model is wanted; the first value of times must be the initial time, i.e. the time of \code{state.init}.
#' @export
#' @note The incidence \code{state.init[["Inc"]]} will be set to 0 at the start of the simulation and computed for each time interval of the vector \code{times}.
#' For instance, if \code{times} contains daily time steps, then the returned data frame will contain daily incidence. 
#' @import deSolve plyr
#' @return a \code{data.fame} containing the simulated trajectories that is the values of the state variables (1 per column) at each observation time (1 per row). The first column is \code{time}.
SEIT2L_simulateDeterministic <- function(theta,state.init,times) {

	SEIT2L_ode <- function(time, state, theta) {

		# param
		beta <- theta[["R0"]]/theta[["IP"]]
		epsilon <- 1/theta[["LP"]]
		nu <- 1/theta[["IP"]]
		alpha <- theta[["alpha"]]
		tau <- 1/theta[["TIP"]]

		# states
		S <- state[["S"]]
		E <- state[["E"]]
		I <- state[["I"]]
		T1 <- state[["T1"]]
		T2 <- state[["T2"]]
		L <- state[["L"]]
		Inc <- state[["Inc"]]

		N <- S + E +I + T1 + T2 + L

		dS <- -beta*S*I/N + (1-alpha)*2*tau*T2
		dE <- beta*S*I/N - epsilon*E
		dI <- epsilon*E - nu*I
		dT1 <- nu*I - 2*tau*T1
		dT2 <- 2*tau*T1 - 2*tau*T2
		dL <- alpha*2*tau*T2
		dInc <- epsilon*E

		return(list(c(dS,dE,dI,dT1,dT2,dL,dInc)))
	}


	# put incidence at 0 in state.init
	state.init["Inc"] <- 0

	traj <- as.data.frame(ode(state.init, times, SEIT2L_ode, theta, method = "ode45"))

	# compute incidence of each time interval
	traj <- mutate(traj,Inc=c(0,diff(Inc)))

	return(traj)

}

#'Stochastic simulation of SEIT2L model
#'
#'Simulate realisation of the stochastic version of the SEIT2L model using the \code{\link{simulateModelStochastic}} function.
#' @inheritParams SEIT2L_simulateDeterministic
#' @note The incidence \code{state.init[["Inc"]]} will be set to 0 at the start of the simulation and computed for each time interval of the vector \code{times}.
#' For instance, if \code{times} contains daily time steps, then the returned data frame will contain daily incidence. 
#' @import plyr
#' @export
#' @seealso \code{\link{fitmodel}}, \code{\link{simulateModelStochastic}}
#' @return a \code{data.fame} containing the simulated trajectories that is the values of the state variables (1 per column) at each observation time (1 per row). The first column is \code{time}.
SEIT2L_simulateStochastic <- function(theta,state.init,times) {

	
	SEIT2L_transitions <- list(
		c(S=-1,E=1),# infection
		c(E=-1,I=1,Inc=1),# infectiousness + incidence
		c(I=-1,T1=1),# recovery + temporary protection
		c(T1=-1,T2=1),# progression of temporary protection
		c(T2=-1,L=1),# efficient long term protection
		c(T2=-1,S=1)# deficient long term protection
		)

	SEIT2L_rateFunc <- function(state,theta,t) {

		# param
		beta <- theta[["R0"]]/theta[["IP"]]
		epsilon <- 1/theta[["LP"]]
		nu <- 1/theta[["IP"]]
		alpha <- theta[["alpha"]]
		tau <- 1/theta[["TIP"]]

		# states
		S <- state[["S"]]
		E <- state[["E"]]
		I <- state[["I"]]
		T1 <- state[["T1"]]
		T2 <- state[["T2"]]
		L <- state[["L"]]
		Inc <- state[["Inc"]]

		N <- S + E +I + T1 + T2 + L

		return(c(
			beta*S*I/N, # infection (S -> E)
			epsilon*E, # infectiousness + incidence (E -> I)
			nu*I, # recovery + short term protection (I -> T1)
			2*tau*T1, # progression of temporary protection (T1 -> T2)
			alpha*2*tau*T2, # efficient long term protection (T2 -> L)
			(1-alpha)*2*tau*T2 # deficient long term protection (T2 -> S)
			)
		)
	}

	# put incidence at 0 in state.init
	state.init["Inc"] <- 0

	traj <- simulateModelStochastic(theta,state.init,times,SEIT2L_transitions,SEIT2L_rateFunc) 
	
	# compute incidence of each time interval
	traj <- mutate(traj,Inc=c(0,diff(Inc)))

	return(traj)

}

#'Create the SEIT2L model as a fitmodel object
#'
#'This function returns a fitmodel object contaning the (either deterministic or stochastic) SEIT2L model, to be fitted to the \code{\link{FluTdC1971}} dataset.
#' @param simulate character. Either \code{"deterministic"} or \code{"stochastic"}.
#' @inheritParams testFitmodel
#' @export
#' @import plyr
#' @return a fitmodel object
SEIT2L_createFitmodel <- function(simulate=c("deterministic","stochastic")) {

	# simulate
	simulate <- match.arg(simulate)

	# simulator
	if(simulate=="deterministic"){
		simulate <- SEIT2L_simulateDeterministic
	} else {
		simulate <- SEIT2L_simulateStochastic
	}

	SEIT2L_name <- "SEIT2L model with daily incidence and constant population size"
	SEIT2L_state.names <- c("S","E","I","T1", "T2","L","Inc")
	SEIT2L_theta.names <- c("R0", "LP", "IP", "alpha", "TIP", "rho")

	# create fitmodel
	SEIT2L <- fitmodel(
		name=SEIT2L_name,
		state.names=SEIT2L_state.names,
		theta.names=SEIT2L_theta.names,
		simulate=simulate,
		genObsPoint=SEITL_genObsPoint,
		logPrior=SEITL_logPrior,
		pointLogLike=SEITL_pointLogLike)

	return(SEIT2L)
}
