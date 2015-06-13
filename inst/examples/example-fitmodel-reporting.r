## Create a simple stochastic SIR model with constant population size
##
## This is based on the determinsitc SIR model, which can be created
## using data(SIR)
##

data(SIR)

SIR_reporting_name <- "SIR with constant population size and incomplete reporting"
SIR_reporting_theta.names <- SIR_theta.names <- c("R0","D_inf", "RR")

## function to compute log-prior
SIR_logPrior <- function(theta, log = FALSE) {

    ## uniform prior on R0: U[1,100]
    log.prior.R0 <- dunif(theta[["R0"]], min = 1, max = 100, log = TRUE)
    ## uniform prior on infectious period: U[0,30]
    log.prior.D_inf <- dunif(theta[["D_inf"]], min = 0, max = 30, log = TRUE)
    ## uniform prior on the reporting rate: U[0,1]
    log.prior.RR <- dunif(theta[["RR"]], min = 0, max = 1, log = TRUE)

    log.sum <- log.prior.R0 + log.prior.D_inf + log.prior.RR

    return(ifelse(log, log.sum, exp(log.sum)))
}

## function to compute the log-likelihood of one data point
SIR_reporting_pointLogLike <- function(data.point, model.point, theta, log = FALSE){

    ## the prevalence is observed through a Poisson process with a reporting rate
    return(dpois(x = data.point[["obs"]],
                 lambda = model.point[["I"]] * theta[["RR"]],
                 log = log))
}

## function to generate observation from a model simulation
SIR_reporting_genObsPoint <- function(model.point, theta){

    ## the prevalence is observed through a Poisson process
    obs.point <- rpois(n = 1, lambda = model.point[["I"]] * theta[["RR"]])

    return(obs.point)
}

## create deterministic SIR fitmodel
SIR_reporting <- fitmodel(
    name = SIR_reporting_name,
    state.names = SIR_state.names,
    theta.names = SIR_theta.names,
    simulate = SIR_simulateDeterministic,
    dprior = SIR_logPrior,
    rPointObs = SIR_reporting_genObsPoint,
    dPointObs = SIR_reporting_pointLogLike)

## test it
## theta <- c(R0=3, D_inf=4, RR=0.7)
## init.state <- c(S = 99,I=1,R=0)

## data(epi)

## SIR_reporting
## testFitmodel(fitmodel = SIR_reporting, theta = theta, init.state = init.state, data = epi2, verbose = TRUE)
