data(SEITL_deter)


SEIT4L_deter_name <- "deterministic SEIT4L model with daily incidence and constant population size"
SEIT4L_state.names <- c("S","E","I","T1", "T2", "T3", "T4","L","Inc")

SEIT4L_simulateDeterministic <- function(theta,init.state,times) {

	SEIT4L_ode <- function(time, state, theta) {

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
		T1 <- state[["T1"]]
		T2 <- state[["T2"]]
		T3 <- state[["T3"]]
		T4 <- state[["T4"]]
		L <- state[["L"]]
		Inc <- state[["Inc"]]

		N <- S + E + I + T1 + T2 + T3 + T4 + L

		dS <- -beta*S*I/N + (1-alpha)*4*tau*T4
		dE <- beta*S*I/N - epsilon*E
		dI <- epsilon*E - nu*I
		dT1 <- nu*I - 4*tau*T1
		dT2 <- 4*tau*T1 - 4*tau*T2
		dT3 <- 4*tau*T2 - 4*tau*T3
		dT4 <- 4*tau*T3 - 4*tau*T4
		dL <- alpha*4*tau*T4
		dInc <- epsilon*E

		return(list(c(dS,dE,dI,dT1,dT2,dT3,dT4,dL,dInc)))
	}


	# put incidence at 0 in init.state
	init.state["Inc"] <- 0

	traj <- as.data.frame(ode(init.state, times, SEIT4L_ode, theta, method = "ode45"))

	# compute incidence of each time interval
	traj <- mutate(traj,Inc=c(0,diff(Inc)))

	return(traj)

}


SEIT4L_deter <- fitmodel(
	name=SEIT4L_deter_name,
	state.names=SEIT4L_state.names,
	theta.names=SEITL_theta.names,
	simulate=SEIT4L_simulateDeterministic,
	genObsPoint=SEITL_genObsPoint,
	logPrior=SEITL_logPrior,
	pointLogLike=SEITL_pointLogLike)


dir_pkg <- "/Users/Tonton/edu/Fit_course/fitR"
save(SEIT4L_deter,file=file.path(dir_pkg,"data","SEIT4L_deter.rdata"))

