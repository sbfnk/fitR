data(SEITL_deter)


SEITL_sto_name <- "stochastic SEITL model with daily incidence and constant population size"

# Simulate realisation of the stochastic version of the SEITL model.
SEITL_simulateStochastic <- function(theta,init.state,times) {

	
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

	# put incidence at 0 in init.state
	init.state["Inc"] <- 0

	traj <- simulateModelStochastic(theta,init.state,times,SEITL_transitions,SEITL_rateFunc) 
	
	# compute incidence of each time interval
	traj <- mutate(traj,Inc=c(0,diff(Inc)))

	return(traj)

}


SEITL_stoch <- fitmodel(
		name=SEITL_sto_name,
		state.names=SEITL_state.names,
		theta.names=SEITL_theta.names,
		simulate=SEITL_simulateStochastic,
		genObsPoint=SEITL_genObsPoint,
		logPrior=SEITL_logPrior,
		pointLogLike=SEITL_pointLogLike)
