data(SEITL_deter)


SEIT4L_sto_name <- "stochastic SEIT4L model with daily incidence and constant population size"
SEIT4L_state.names <- c("S","E","I","T1", "T2", "T3", "T4","L","Inc")

# Simulate realisation of the stochastic version of the SEIT4L model.
SEIT4L_simulateStochastic <- function(theta,init.state,times) {

	
	SEIT4L_transitions <- list(
		c(S=-1,E=1),# infection
		c(E=-1,I=1,Inc=1),# infectiousness + incidence
		c(I=-1,T1=1),# recovery + temporary protection
		c(T1=-1,T2=1),# progression of temporary protection
		c(T2=-1,T3=1),# progression of temporary protection
		c(T3=-1,T4=1),# progression of temporary protection
		c(T4=-1,L=1),# efficient long term protection
		c(T4=-1,S=1)# deficient long term protection
		)

	SEIT4L_rateFunc <- function(state,theta,t) {

		# param
		beta <- theta[["R0"]]/theta[["D_inf"]]
		epsilon <- 1/theta[["D_lat"]]
		nu <- 1/theta[["D_inf"]]
		alpha <- theta[["alpha"]]
		tau <- 1/theta[["D_imm"]]

		# states
		S <- state[["S"]]
		E <- state[["E"]]
		I <- state[["I"]]
		T1 <- state[["T1"]]
		T2 <- state[["T2"]]
		T3 <- state[["T3"]]
		T4 <- state[["T4"]]
		L <- state[["L"]]
		Inc <- state[["Inc"]]

		N <- S + E +I + T1 + T2 + T3 + T4 + L

		return(c(
			beta*S*I/N, # infection (S -> E)
			epsilon*E, # infectiousness + incidence (E -> I)
			nu*I, # recovery + short term protection (I -> T1)
			4*tau*T1, # progression of temporary protection (T1 -> T2)
			4*tau*T2, # progression of temporary protection (T2 -> T3)
			4*tau*T3, # progression of temporary protection (T3 -> T4)
			alpha*4*tau*T4, # efficient long term protection (T4 -> L)
			(1-alpha)*4*tau*T4 # deficient long term protection (T4 -> S)
			)
		)
	}

	# put incidence at 0 in init.state
	init.state["Inc"] <- 0

	traj <- simulateModelStochastic(theta,init.state,times,SEIT4L_transitions,SEIT4L_rateFunc) 
	
	# compute incidence of each time interval
	traj <- mutate(traj,Inc=c(0,diff(Inc)))

	return(traj)

}


SEIT4L_stoch <- fitmodel(
	name=SEIT4L_sto_name,
	state.names=SEIT4L_state.names,
	theta.names=SEITL_theta.names,
	simulate=SEIT4L_simulateStochastic,
	dprior=SEITL_prior,
	rPointObs=SEITL_genObsPoint,
	dPointObs=SEITL_pointLike)
