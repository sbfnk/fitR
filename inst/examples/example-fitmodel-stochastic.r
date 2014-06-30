# Create a simple stochastic SIR model with constant population size
#
# This is based on the determinsitc SIR model, which can be created
# using example(SIR)

SIR_stochastic_name <- "stochastic SIR with constant population size"

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

        # make use of the function simulateModelStochastic that
        # returns trajectories in the correct format
	return(simulateModelStochastic(theta,state.init,times,SIR_transitions,SIR_rateFunc))

}

# create stochastic SIR fitmodel
SIR_stoch <- fitmodel(
        name=SIR_stochastic_name,
        state.names=SIR_state.names,
        theta.names=SIR_theta.names,
        simulate=SIR_simulateStochastic,
        genObsPoint=SIR_genObsPoint,
        logPrior=SIR_logPrior,
        pointLogLike=SIR_pointLogLike)

# test it
theta <- c(R0=3, D=4)
state.init <- c(S=99,I=1,R=0)

# SIR_stoch
# testFitmodel(fitmodel=SIR_stoch, theta=theta, state.init=state.init, data= data, verbose=TRUE)


