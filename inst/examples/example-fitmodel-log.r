# Create a simple stochastic SIR model with constant population size
# and parameters on the exponential scale
#
# This is based on the determinstic SIR model, which can be created
# using data(SIR)
#

data(SIR)

SIR_exp_name <- "SIR with constant population size, parameters transformed to the exponential scale"

SIR_exp_simulateDeterministic <- function(theta,init.state,times) {

        SIR_ode <- function(time, state, parameters) {

                ## parameters
                beta <- exp(parameters[["R0"]]) / exp(parameters[["D_inf"]])
                nu <- 1 / exp(parameters[["D_inf"]])

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

	trajectory <- data.frame(ode(y=init.state,times=times,func=SIR_ode,parms=theta, method = "ode45"))

	return(trajectory)
}

## function to compute log-prior
SIR_exp_logPrior <- function(theta, log = FALSE) {

    ## uniform prior on R0: U[1,100]
    log.prior.R0 <- dunif(exp(theta[["R0"]]), min = 1, max = 100, log = TRUE)
    ## uniform prior on infectious period: U[0,30]
    log.prior.D <- dunif(exp(theta[["D_inf"]]), min = 0, max = 30, log = TRUE)

    log.sum <- log.prior.R0 + log.prior.D

    return(ifelse(log, log.sum, exp(log.sum)))
}

## create deterministic SIR fitmodel
SIR_exp <- fitmodel(
	name=SIR_exp_name,
        state.names=SIR$state.names,
	theta.names=SIR$theta.names,
        simulate=SIR_exp_simulateDeterministic,
	dprior=SIR_exp_logPrior,
	rPointObs=SIR$rPointObs,
	dPointObs=SIR$dPointObs)

## test them
theta <- c(R0 = log(3), D_inf = log(2))
init.state <- c(S = 999, I = 1, R = 0)
data(epi)
