# create a simple stochastic SIR model with constant population size
SIR_name <- "SIR with constant population size"
SIR_state.names <- c("S","I","R")
SIR_theta.names <- c("R0","D","reporting.rate")

SIR_simulateDeterministic <- function(theta,state.init,times) {

        SIR_ode <- function(time, state, parameters) {

                ## parameters
                beta <- parameters[["R0"]] / parameters[["D"]]
                nu <- 1 / parameters[["D"]]

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

SIR_simulateStochastic <- function(theta,state.init,times) {

        ## transitions
        SIR_transitions <- list(
                c(S = -1, I = 1), # infection
                c(I = -1, R = 1) # recovery
        )

        ## rates
        SIR_rateFunc <- function(x, parameters, t) {

                beta <- parameters[["R0"]]/parameters[["D"]]
                nu <- 1/parameters[["D"]]

                S <- x[["S"]]
                I <- x[["I"]]
                R <- x[["R"]]

                N <- S + I + R

                return(c(
                        beta * S * I / N, # infection
                        nu * I # recovery
                ))
        }

        ## make use of the function simulateModelStochastic that returns trajectories in the correct format
	return(simulateModelStochastic(theta,state.init,times,SIR_transitions,SIR_rateFunc))

}

## function to compute log-prior
SIR_logPrior <- function(theta) {

        ## uniform prior on R0: U[1,100]
        log.prior.R0 <- dunif(theta[["R0"]], min = 1, max = 100, log = TRUE)
        ## uniform prior on infectious period: U[0,30]
        log.prior.D <- dunif(theta[["D"]], min = 0, max = 30, log = TRUE)

	return(log.prior.R0 + log.prior.D)
}

## function to compute the log-likelihood of one data point
SIR_pointLogLike <- function(data.point, model.point, theta){

        ## the prevalence is observed through a Poisson process with a reporting rate
	return(dpois(x=data.point[["obs"]], lambda=model.point[["I"]], log=TRUE))
}

## function to generate observation from a model simulation
SIR_genObsPoint <- function(model.point, theta){

        ## the prevalence is observed through a Poisson process with a reporting rate
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

## create stochastic SIR fitmodel
SIR_sto <- fitmodel(
        name=SIR_name,
        state.names=SIR_state.names,
        theta.names=SIR_theta.names,
        simulate=SIR_simulateStochastic,
        genObsPoint=SIR_genObsPoint,
        logPrior=SIR_logPrior,
        pointLogLike=SIR_pointLogLike)

## test them
theta <- c(R0=3, D=4, reporting.rate=0.7)
state.init <- c(S=99,I=1,R=0)
data <- data.frame(time=1:5,I=1:5)

## SIR_deter
testFitmodel(fitmodel=SIR_deter, theta=theta, state.init=state.init, data= data, verbose=TRUE)

## SIR_sto
testFitmodel(fitmodel=SIR_sto, theta=theta, state.init=state.init, data= data, verbose=TRUE)


