# create a simple deterministic SIR model with constant population size

SIR_name <- "SIR with constant population size"
SIR_state.names <- c("S","I","R")
SIR_theta.names <- c("R0","D.inf")

SIR_simulateDeterministic <- function(theta,state.init,times) {

        SIR_ode <- function(time, state, parameters) {

                ## parameters
                beta <- parameters[["R0"]] / parameters[["D.inf"]]
                nu <- 1 / parameters[["D.inf"]]

                ## states
                S <- state[["S"]]
                I <- state[["I"]]
                R <- state[["R"]]

                N <- S + I + R

                dS <- -beta * S * I/N
                dI <- beta * S * I/N - nu * I
                dR <- nu * I

                return(list(c(dS, dI, dR)))
        }

	trajectory <- data.frame(ode(y=state.init,times=times,func=SIR_ode,parms=theta, method = "ode45"))

	return(trajectory)
}

## function to compute log-prior
SIR_logPrior <- function(theta) {

        ## uniform prior on R0: U[1,100]
        log.prior.R0 <- dunif(theta[["R0"]], min = 1, max = 100, log = TRUE)
        ## uniform prior on infectious period: U[0,30]
        log.prior.D <- dunif(theta[["D.inf"]], min = 0, max = 30, log = TRUE)

	return(log.prior.R0 + log.prior.D)
}

## function to compute the log-likelihood of one data point
SIR_pointLogLike <- function(data.point, model.point, theta){

        ## the prevalence is observed through a Poisson process
	return(dpois(x=data.point[["obs"]], lambda=model.point[["I"]], log=TRUE))
}

## function to generate observation from a model simulation
SIR_genObsPoint <- function(model.point, theta){

        ## the prevalence is observed through a Poisson process
        obs.point <- rpois(n=1, lambda=model.point[["I"]])

        return(obs.point)
}

## create deterministic SIR fitmodel
SIR <- fitmodel(
	name=SIR_name,
        state.names=SIR_state.names,
	theta.names=SIR_theta.names,
        simulate=SIR_simulateDeterministic,
	genObsPoint=SIR_genObsPoint,
	logPrior=SIR_logPrior,
	pointLogLike=SIR_pointLogLike)

## test them
theta <- c(R0 = 3, D.inf = 2)
state.init <- c(S = 999, I = 1, R = 0)
data(epi)

## SIR
## testFitmodel(fitmodel=SIR, theta=theta, state.init=state.init, data= epi1, verbose=TRUE)

