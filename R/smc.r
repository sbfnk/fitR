#'Run a particle filter for fitmodel object
#'
#'The particle filter returns an estimate of the marginal log-likelihood \eqn{L = p(y(t_{1:T})|\theta)}
#'as well as the set of filtered trajectories and their respective weights at the last observation time \eqn{\omega(t_T)=p(y(t_T)|\theta)}.
#' @param n.particles number of particles
#' @param progress if \code{TRUE} progression of the filter is displayed in the console.
#' @param n.cores number of cores on which propogation of the particles is parallelised. By default no parallelisation (\code{n.cores=1}). If \code{NULL}, set to the value returned by \code{\link[parallel]{detectCores}}.
#' @inheritParams testFitmodel
#' @note An unbiased state sample \eqn{x(t_{0:T}) ~ p(X(t_{0:T})|\theta,y(t_{0:T}))} can be obtained by sampling the set of trajectories \code{traj} with probability \code{traj.weight}.
#' @export
#' @seealso plotSMC
#' @import parallel doParallel
#' @return A list of 3 elements:
#' \itemize{
#' \item \code{pointLogLike} the marginal log-likelihood of the theta.
#' \item \code{traj} a list of size \code{n.particles} with all filtered trajectories.
#' \item \code{traj.weight} a vector of size \code{n.particles} with the normalised weight of the filtered trajectories.
#' }
particleFilter <- function(fitmodel, theta, init.state, data, n.particles, progress = FALSE, n.cores = 1)
{

    if(is.null(n.cores)){
        n.cores <- detectCores()
            # cat("SMC runs on ",n.cores," cores\n")
    }

    if(n.cores > 1){
        registerDoParallel(cores=n.cores)
    }

    ## compute the margLogLike using a particle filter

    # initialisation

    # marginal log-likelihood of theta
    margLogLike <- 0

    # initial state of particles
    current.state.particles <- rep(list(init.state),n.particles)

    # filtered trajectories (just add time variable to initial state)
    traj.particles <- rep(list(data.frame(t(c(time=0,init.state)))),n.particles)

    # weight of particles
    weight.particles <- rep(1/n.particles,length=n.particles)

    if(progress){
        # help to visualise progression of the filter
        progress.bar <- txtProgressBar(min=1, max= nrow(data))
    }

    # particle filter
    for(i in seq_len(nrow(data))){

        # initial + observation times
        times <- c(ifelse(i==1,0,data$time[i-1]),data$time[i])
        data.point <- unlist(data[i, ]) # must be a vector

        if(!all(weight.particles==0)){
            # resample particles according to their weight (normalization is done in the function sample())
            index.resampled <- sample(x=n.particles,size=n.particles,replace=TRUE,prob=weight.particles)
        }else{
            warning("All particles depleted at step ",i," of SMC. Return margLogLike = -Inf for theta: ",printNamedVector(theta), call.=FALSE)
            return(list(margLogLike=-Inf,traj=NA,traj.weight=NA))
        }

        # update traj and current state after resampling
        traj.particles <- traj.particles[index.resampled]
        current.state.particles <- current.state.particles[index.resampled]

        # propagate particles (this for loop could be parallelized)
        propagate <- llply(current.state.particles,function(current.state) {

            # simulate from previous observation to current observation time
            traj <- fitmodel$simulate(theta=theta,init.state=current.state,times=times)

            # compute particle weight
            model.point <- unlist(traj[2,fitmodel$state.names])
            weight <- exp(fitmodel$pointLogLike(data.point=data.point, model.point=model.point, theta=theta))

            return(list(state=model.point,weight=weight))

        },.parallel=(n.cores > 1))

        # collect parallel jobs
        current.state.particles <- llply(propagate,function(x) {x$state})
        weight.particles <- unlist(llply(propagate,function(x) {x$weight}))
        traj.particles <- llply(seq_along(propagate),function(j) {rbind(traj.particles[[j]],c(data.point["time"],propagate[[j]]$state))})

        # update marginal log-likelihood
        margLogLike <- margLogLike + log(mean(weight.particles))

        if(progress){
            # advance progress bar
            setTxtProgressBar(progress.bar, i)
        }
    }

    if(progress){
        close(progress.bar)
    }

    # return marginal log-likelihood, filtered trajectories, normalised weight of each trajectory
    ans <- list(margLogLike=margLogLike,traj=traj.particles,traj.weight=weight.particles/sum(weight.particles))

    return(ans)

}
