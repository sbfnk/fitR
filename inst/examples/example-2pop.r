## create a simple deterministic SIR model with constant population size

SIR_2pop_name <- "SIR with two populations interacting"
SIR_2pop_state.names <- c("S_y","I_y","R_y","S_a","I_a","R_a")
SIR_2pop_theta.names <- c("R_yy","R_aa","R_ay","D_inf")

SIR_2pop_simulateDeterministic <- function(theta,init.state,times) {

    SIR_2pop_ode <- function(time, state, parameters) {

        
        ## states
        S_y <- state[["S_y"]]
        I_y <- state[["I_y"]]
        R_y <- state[["R_y"]]
        S_a <- state[["S_a"]]
        I_a <- state[["I_a"]]
        R_a <- state[["R_a"]]

        N_y <- S_y + I_y + R_y
        N_a <- S_a + I_a + R_a

        ## parameters
        beta_aa <- parameters[["R_aa"]] / (parameters[["D_inf"]]*N_a)
        beta_yy <- parameters[["R_yy"]] / (parameters[["D_inf"]]*N_y)
        beta_ay <- parameters[["R_ay"]] / (parameters[["D_inf"]]*N_a)
        beta_ya <- beta_ay
        nu <- 1 / parameters[["D_inf"]]

        lambda_y <- beta_yy * I_y + beta_ya * I_a
        lambda_a <- beta_aa * I_a + beta_ay * I_y

        dS_y <- - lambda_y * S_y
        dI_y <- lambda_y * S_y - nu * I_y
        dR_y <- nu * I_y

        dS_a <- - lambda_a * S_a
        dI_a <- lambda_a * S_a - nu * I_a
        dR_a <- nu * I_a

        return(list(c(dS_y, dI_y, dR_y, dS_a, dI_a, dR_a)))
    }

    trajectory <- data.frame(ode(y = init.state,
     times = times,
     func = SIR_2pop_ode,
     parms = theta,
     method = "ode45"))

    return(trajectory)
}

# TODO
## function to ypute log-prior
SIR_2pop_prior <- function(theta, log = FALSE) {

    ## uniform prior on R_xx: U[1,100]
    log.prior.R_aa <- dunif(theta[["R_aa"]], min = 0, max = 100, log = TRUE)
    log.prior.R_yy <- dunif(theta[["R_yy"]], min = 0, max = 100, log = TRUE)
    log.prior.R_ay <- dunif(theta[["R_ay"]], min = 0, max = 100, log = TRUE)
    log.prior.R_ya <- dunif(theta[["R_ya"]], min = 0, max = 100, log = TRUE)
    ## uniform prior on infectious period: U[0,30]
    log.prior.D <- dunif(theta[["D_inf"]], min = 0, max = 30, log = TRUE)

    log.sum <- log.prior.R_aa + log.prior.R_yy + log.prior.R_ay + log.prior.R_ya + log.prior.D

    return(ifelse(log, log.sum, exp(log.sum)))
}

## function to ypute the likelihood of one data point
SIR_2pop_pointLike <- function(data.point, model.point, theta, log = FALSE){

    ## the prevalence is observed through a Poisson process
    logLike_y <- dpois(x = data.point[["obs_y"]],
     lambda = model.point[["I_y"]],
     log = TRUE)

    logLike_a <- dpois(x = data.point[["obs_a"]],
     lambda = model.point[["I_a"]],
     log = TRUE)

    log.sum <- logLike_y + logLike_a

    return(ifelse(log, log.sum, exp(log.sum)))
}

## function to generate observation from a model simulation
SIR_2pop_genObsPoint <- function(model.point, theta){

    ## the prevalence is observed through a Poisson process
    obs.point_y <- rpois(n = 1, lambda = model.point[["I_y"]])
    obs.point_a <- rpois(n = 1, lambda = model.point[["I_a"]])

    return(c(obs_y = obs.point_y, obs_a = obs.point_y))
}

## create deterministic SIR fitmodel
SIR_2pop <- fitmodel(
    name = SIR_2pop_name,
    state.names = SIR_2pop_state.names,
    theta.names = SIR_2pop_theta.names,
    simulate = SIR_2pop_simulateDeterministic,
    dprior = SIR_2pop_prior,
    rPointObs = SIR_2pop_genObsPoint,
    dPointObs = SIR_2pop_pointLike)

## test
theta <- c("R_yy"=1.4,"R_aa"=1.1,"R_ay"=0.5,"D_inf"=3)
init.state <- c("S_y"=99,"I_y"=1,"R_y"=0,"S_a"=200,"I_a"=0,"R_a"=0)

# data(epi)


# x <- SIR_2pop$simulate(theta, init.state, times=1:60)
# plotTraj(x)





