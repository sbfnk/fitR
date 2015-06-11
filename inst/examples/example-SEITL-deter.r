
SEITL_deter_name <- "deterministic SEITL model with daily incidence and constant population size"
# note the new state Inc for the daily incidence
SEITL_state.names <- c("S","E","I","T","L","Inc")
SEITL_theta.names <- c("R0", "D_lat", "D_inf", "alpha", "D_imm", "rho")


# Solves the system of ordinary differential equations for the SEITL model.
SEITL_simulateDeterministic <- function(theta,init.state,times) {

	SEITL_ode <- function(time, state, theta) {

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


	# put incidence at 0 in init.state
	init.state["Inc"] <- 0

	traj <- as.data.frame(ode(init.state, times, SEITL_ode, theta, method = "ode45"))

	# compute incidence of each time interval
	traj <- mutate(traj,Inc=c(0,diff(Inc)))

	return(traj)

}


# Generate an observed incidence under a Poisson observation process.  
SEITL_genObsPoint <- function(model.point, theta){

	obs.point <- rpois(n=1, lambda=theta[["rho"]]*model.point[["Inc"]])

	return(c(obs=obs.point))
}

# Evaluate the (log of the) prior density distribution of the parameter values.
SEITL_prior <- function(theta, log = FALSE) {

	log.prior.R0 <- dunif(theta[["R0"]], min = 1, max = 50, log = log)
	log.prior.latent.period <- dunif(theta[["D_lat"]], min = 0, max = 10, log = log)
	log.prior.infectious.period <- dunif(theta[["D_inf"]], min = 0, max = 15, log = log)
	log.prior.temporary.immune.period <- dunif(theta[["D_imm"]], min = 0, max = 50, log = log)
	log.prior.probability.long.term.immunity <- dunif(theta[["alpha"]], min = 0, max = 1, log = log)
	log.prior.reporting.rate <- dunif(theta[["rho"]], min = 0, max = 1, log = log)
	
	return(log.prior.R0 + log.prior.latent.period + log.prior.infectious.period + log.prior.temporary.immune.period + log.prior.probability.long.term.immunity + log.prior.reporting.rate)

}


# Computes the (log)-likelihood of a data point given the state of the model and under a poisson observation process.
SEITL_pointLike <- function(data.point, model.point, theta, log = FALSE){

	return(dpois(x=data.point[["obs"]],lambda=theta[["rho"]]*model.point[["Inc"]],log=log))

}


# create fitmodel
SEITL_deter <- fitmodel(
	name=SEITL_deter_name,
	state.names=SEITL_state.names,
	theta.names=SEITL_theta.names,
	simulate=SEITL_simulateDeterministic,
	dprior=SEITL_prior,
	rPointObs=SEITL_genObsPoint,
	dPointObs=SEITL_pointLike)

## test it

# theta <- c("R0"=10, "D_lat"=2 , "D_inf"=3, "alpha"=0.5, "D_imm"=15, "rho"=0.7)
# init.state <- c("S"=280,"E"=0,"I"=2,"T"=0,"L"=4,"Inc"=0)
# data(FluTdC1971)
# testFitmodel(fitmodel=SEITL, theta=theta, init.state=init.state, data= FluTdC1971, verbose=TRUE)


