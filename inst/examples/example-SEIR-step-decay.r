
SE2IR_stepDecay_deter_name <- "deterministic SEIR model with constant population size, erlang distribution for E (shape=2) and step decay of reproduction number"
SE2IR_stepDecay_state.names <- c("S","E1", "E2", "I","R","Inc")
SE2IR_stepDecay_theta.names <- c("R0", "D_lat", "D_inf", "rho", "t_intervention", "alpha")

calculate_R_t <- function(time, R0, t_intervention, alpha) {

	R_t <- rep(R0, len = length(time))

	R_t[time >= t_intervention] <- R0*(1 - alpha)

	return(R_t)

}
# Solves the system of ordinary differential equations for the SE2IR_stepDecay model.
SE2IR_stepDecay_simulateDeterministic <- function(theta,init.state,times) {

	SE2IR_stepDecay_ode <- function(time, state, theta) {

		# param

		R_t <- calculate_R_t(time, theta[["R0"]], theta[["t_intervention"]], theta[["alpha"]])

		beta_t <- R_t/theta[["D_inf"]]
		epsilon <- 1/theta[["D_lat"]]
		nu <- 1/theta[["D_inf"]]

		# states
		S <- state[["S"]]
		E1 <- state[["E1"]]
		E2 <- state[["E2"]]
		I <- state[["I"]]
		R <- state[["R"]]
		Inc <- state[["Inc"]]

		N <- S + E1 + E2 +I + R

		dS <- -beta_t*S*I/N
		dE1 <- beta_t*S*I/N - 2*epsilon*E1
		dE2 <- 2*epsilon*(E1-E2)
		dI <- 2*epsilon*E2 - nu*I
		dR <- nu*I
		dInc <- 2*epsilon*E2

		return(list(c(dS,dE1,dE2,dI,dR,dInc)))
	}


	# put incidence at 0 in init.state
	init.state["Inc"] <- 0

	traj <- as.data.frame(ode(init.state, times, SE2IR_stepDecay_ode, theta, method = "ode45"))

	# compute incidence of each time interval
	traj$Inc <- c(0, diff(traj$Inc))

	return(traj)

}


# Generate an observed incidence under a Poisson observation process.  
SE2IR_stepDecay_genObsPoint <- function(model.point, theta){

	obs.point <- rpois(n=1, lambda=theta[["rho"]]*model.point[["Inc"]])

	return(c(obs=obs.point))
}

# Evaluate the (log of the) prior density distribution of the parameter values.
SE2IR_stepDecay_prior <- function(theta, log = FALSE) {

	log.prior.R0 <- dunif(theta[["R0"]], min = 1, max = 50, log = TRUE)
	log.prior.latent.period <- log(dtruncnorm(theta[["D_lat"]], a = 0, b = Inf, mean = 9/7, sd = 2/7))
	log.prior.infectious.period <- log(dtruncnorm(theta[["D_inf"]], a = 0, b = Inf, mean = 9/7, sd = 2/7))
	log.prior.t_intervention <- dunif(theta[["t_intervention"]], min = 0, max = 300, log = TRUE)
	log.prior.step.decay <- dunif(theta[["alpha"]], min = 0, max = 1, log = TRUE)
	log.prior.reporting.rate <- dunif(theta[["rho"]], min = 0, max = 1, log = TRUE)

	log.sum = log.prior.R0 + log.prior.latent.period + log.prior.infectious.period + log.prior.t_intervention + log.prior.step.decay + log.prior.reporting.rate
	
	return(ifelse(log, log.sum, step(log.sum)))

}


# Computes the (log)-likelihood of a data point given the state of the model and under a poisson observation process.
SE2IR_stepDecay_pointLike <- function(data.point, model.point, theta, log = FALSE){

	return(dpois(x=data.point[["obs"]],lambda=theta[["rho"]]*model.point[["Inc"]],log=log))

}


# create fitmodel
SE2IR_stepDecay_deter <- fitmodel(
	name=SE2IR_stepDecay_deter_name,
	state.names=SE2IR_stepDecay_state.names,
	theta.names=SE2IR_stepDecay_theta.names,
	simulate=SE2IR_stepDecay_simulateDeterministic,
	dprior=SE2IR_stepDecay_prior,
	rPointObs=SE2IR_stepDecay_genObsPoint,
	dPointObs=SE2IR_stepDecay_pointLike)

## test it

## theta <- c("R0"=1.5, "D_lat"=9/7, "D_inf"=10/7, "t_intervention" = 25, "alpha"=0.5, "rho"=0.7)
## init.state <- c("S"=99995,"E1"=0, "E2"=0,"I"=5,"R"=0,"Inc"=0)
## # data(FluTdC1971) 
## # testFitmodel(fitmodel=SE2IR_stepDecay, theta=theta, init.state=init.state, data= FluTdC1971, verbose=TRUE)


## times <- 1:65
## Rt <- calculate_R_t(times, theta[["R0"]], theta[["t_intervention"]], theta[["alpha"]])

## quartz()
## plot(times, Rt, t='l')

## # df_traj <- SE2IR_stepDecay_deter$simulate(theta, init.state, times=1:70)
## df_traj <- rTrajObs(SE2IR_stepDecay_deter, theta, init.state, times=1:65)

## quartz()
## plotTraj(df_traj)


## df_data_step_decay_noisy <- df_traj %>% select(time, obs) %>% mutate(time = time - 1) %>% filter(time > 0)
## df_data_step_decay <- df_traj %>% mutate(time = time - 1, obs = round(0.7*Inc)) %>% select(time, obs)  %>% filter(time > 0)

## library(readr)
## # write_csv(df_data_step_decay, file.path("/Users/Tonton/edu/Fit_course/mfiidd/website/data","ebola_dataset_3.csv")) #dataset3
## # write_csv(df_data_step_decay_noisy, file.path("/Users/Tonton/edu/Fit_course/mfiidd/website/data","ebola_dataset_4.csv")) #dataset4



## # df_plot <- df_data_step_decay_noisy
## # ggplot(df_plot, aes(x=time, y=obs)) + geom_line()

