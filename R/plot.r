#'Plot one or more model simulations
#'
#'This function use faceting to plot one or more trajectories of all state variables. Convenient to see results of several simulations. Also, if \code{data} is present, then an additional plot
#' @param traj data.frame, output of \code{fitmodel$simulateTraj} or \code{simulateModelReplicates}.
#' @param state.names character vector. Names of the state variables to plot. Names must match \code{fitmodel$state.names}. If \code{NULL} (default) all state variables are plotted.
#' @param data data frame. Observation times and observed data. The time column must be named \code{time}, whereas the name of the data column should match one of \code{fitmodel$state.names}. 
#' @param summary logical. If \code{TRUE}, the mean, median as well as the 50th and 95th percentile of the trajectories are plotted (default). If \code{FALSE}, all individual trajectories are plotted (transparency can be set with \code{alpha}).
#' @param alpha transparency of the trajectories (between 0 and 1).
#' @param plot if \code{TRUE} the plot is displayed, and returned otherwise.
#' @export
#' @import reshape2 ggplot2
#' @seealso \code{\link{simulateModelReplicates}}
plotTraj <- function(traj, state.names=NULL, data=NULL, summary=TRUE, alpha=1, plot=TRUE) {

    if(!"replicate"%in%names(traj) && !any(duplicated(traj$time))){
        traj$replicate <- 1

        if(summary){
            message("Only 1 replicate to summarise: mean, median and CI of the trajectories won't be plotted.")
            summary <- FALSE
        }
    }

    if(is.null(state.names)){
        state.names <- setdiff(names(traj),c("time","replicate"))
    }

    df.traj <- melt(traj,measure.vars=state.names,variable.name="state")

    if(summary){

        message("Compute confidence intervals")

        traj.CI <- ddply(df.traj,c("time","state"),function(df) {

            tmp <- as.data.frame(t(quantile(df$value,prob=c(0.025,0.25,0.5,0.75,0.975))))
            names(tmp) <- c("low_95","low_50","median","up_50","up_95")
            tmp$mean <- mean(df$value)
            return(tmp)

        },.progress="text")

        traj.CI.line <- melt(traj.CI[c("time","state","mean","median")],id.vars=c("time","state"))
        traj.CI.area <- melt(traj.CI[c("time","state","low_95","low_50","up_50","up_95")],id.vars=c("time","state"))
        traj.CI.area$type <- sapply(traj.CI.area$variable,function(x) {str_split(x,"_")[[1]][1]})
        traj.CI.area$CI <- sapply(traj.CI.area$variable,function(x) {str_split(x,"_")[[1]][2]})
        traj.CI.area$variable <- NULL
        traj.CI.area <- dcast(traj.CI.area,"time+state+CI~type")

        p <- ggplot(traj.CI.area)+facet_wrap(~state, scales="free_y")
        p <- p + geom_ribbon(data=traj.CI.area,aes(x=time,ymin=low,ymax=up,alpha=CI),fill="red")
        p <- p + geom_line(data=traj.CI.line,aes(x=time,y=value,linetype=variable),colour="red")
        p <- p + scale_alpha_manual("Percentile",values=c("95"=0.25,"50"=0.45),labels=c("95"="95th","50"="50th"))
        p <- p + scale_linetype("Stats")
        p <- p + guides(linetype = guide_legend(order = 1))

    } else {

        p <- ggplot(df.traj)+facet_wrap(~state, scales="free_y")
        p <- p + geom_line(data=df.traj,aes(x=time,y=value,group=replicate),alpha=alpha,colour="red")

    }

    if(!is.null(data)){

        # data are present, find which variable to plot by matching names of fitmodel states
        data.obs.name <- intersect(names(data), state.names)
        if(!length(data.obs.name)){
            warnings("Names of ",sQuote("data")," don't match any state variable names of ",sQuote("traj"),": data won't be plotted.")
        } else {
            obs.name <- "observation"
            names(obs.name) <- data.obs.name
            data <- rename(data, obs.name)
            data <- melt(data, measure.vars=obs.name,variable.name="state")

            p <- p + geom_point(data=data,aes(x=time,y=value),colour="black")
        }
    }

    p <- p + theme_bw()

    if(plot){
        print(p)
    }else{
        return(p)
    }

}



#'Plot fit of model to data
#'
#'This function simulates the model under \code{theta}, generates observation and plot them against the data. Since simulation and observation processes can be stochastic, \code{n.replicates} can be plotted.
#' @param n.replicates numeric, number of replicated simulations.
#' @param only.fit logical, if \code{TRUE} only the observations are plotted. Otherwise, all states are plotted.
#' @inheritParams margLogLikeDeter
#' @inheritParams plotTraj
#' @export
#' @import plyr ggplot2 
#' @return if \code{plot==FALSE}, a list of 2 elements is returned:
#' \itemize{
#'     \item \code{simulations} \code{data.frame} of \code{n.replicates} simulated observations.
#'     \item \code{plot} the plot of the fit.
#' }
plotFit <- function(fitmodel, theta, state.init, data, n.replicates=1, summary=TRUE, alpha=min(1,10/n.replicates), only.fit=TRUE, plot=TRUE) {

    times <- c(0, data$time)

    cat("Simulate ",n.replicates," replicate(s)\n")
    traj <- simulateModelReplicates(fitmodel=fitmodel,theta=theta, state.init=state.init, times=times, n=n.replicates, observation=TRUE)

    if(only.fit){
        state.names <- c("observation")
    } else {
        state.names <- NULL
    }

    p <- plotTraj(traj=traj, state.names=state.names, data=data, summary=summary, alpha=alpha, plot=FALSE)

    if(plot){
        print(p)        
    } else {
        return(list(traj=traj,plot=p))        
    }

}


#'Plot result of SMC
#'
#'Plot the observation generated by the filtered trajectories together with the data.
#' @param smc output of \code{\link{particleFilter}}
#' @inheritParams plotTraj
#' @inheritParams plotFit
#' @export
#' @import ggplot2 plyr
#' @seealso particleFilter
plotSMC <- function(smc, fitmodel, theta, data=NULL, summary=TRUE, alpha=1, only.fit=TRUE, plot=TRUE) {

    traj <- smc$traj
    names(traj) <- 1:length(traj)

    traj <- ldply(traj,function(df) {
        return(fitmodel$generate.observation(df,theta))
    },.id="replicate")

    if(only.fit){
        state.names <- c("observation")
    } else {
        state.names <- NULL
    }

    p <- plotTraj(traj=traj, state.names=state.names, data=data, summary=summary, alpha=alpha, plot=FALSE)


    if(plot){
        print(p)
    }else{
        return(p)
    }

}


#'Plot MCMC trace
#'
#'Plot the traces of all estimated variables.
#' @param trace a \code{data.frame} with one column per estimated parameter, as returned by \code{\link{burnAndThin}}
#' @param estimated.only logical, if \code{TRUE} only estimated parameters are displayed.
#' @export
#' @import ggplot2 reshape2
#' @seealso burnAndThin
plotTrace <- function(trace, estimated.only = FALSE){

    if(estimated.only){
        is.fixed <- apply(trace,2,function(x) {length(unique(x))==1})
        trace <- trace[,-which(is.fixed)]
    }

    df <- melt(trace,id.vars="iteration")

    # density
    p <- ggplot(df,aes(x=iteration,y=value))+facet_wrap(~variable,scales="free")
    p <- p+geom_line(alpha=0.75)
    print(p)

}

#'Plot MCMC posterior densities
#'
#'Plot the posterior density of all estimated variables.
#' @inheritParams plotTrace
#' @export
#' @import ggplot2 reshape2
#' @seealso burnAndThin
plotPosteriorTheta <- function(trace, estimated.only = FALSE){

    if(estimated.only){
        is.fixed <- apply(trace,2,function(x) {length(unique(x))==1})
        trace <- trace[,-which(is.fixed)]
    }


    df <- melt(trace,id.vars="iteration")

    # density
    p <- ggplot(df,aes(x=value))+facet_wrap(~variable,scales="free")
    p <- p+geom_histogram(aes(y=..density..),alpha=0.75)
    p <- p+geom_density()
    print(p)

}


#'Plot MCMC posterior fit
#'
#'Plot posterior distribution of observation generated under model's posterior parameter distribution.
#' @param posterior.summary character. Set to \code{"sample"} to plot trajectories from a sample of the posterior (default). Set to \code{"median"}, \code{"mean"} or \code{"max"} to plot trajectories corresponding to the median, mean and maximum of the posterior density.
#' @param summary logical, if \code{TRUE} trajectories are summarised by their mean, median, 50\% and 95\% quantile distributions. Otheriwse, the trajectories are ploted.
#' @param sample.size number of theta sampled from posterior distribution (if \code{posterior.summary=="sample"}). Otherwise, number of replicated simulations.
#' @inheritParams testFitmodel
#' @inheritParams plotTrace
#' @inheritParams plotTraj
#' @export
#' @import ggplot2 plyr
#' @return If \code{plot==FALSE}, a list of 2 elements is returned:
#'\itemize{
#'    \item \code{posterior.traj} a \code{data.frame} with the trajectories (and observations) sampled from the posterior distribution.
#'    \item \code{plot} the plot of the fit displayed.
#'}
plotPosteriorFit <- function(trace, fitmodel, theta, state.init, posterior.summary=c("sample","median","mean","max"), summary=TRUE, sample.size = 100, alpha=min(1,10/sample.size), plot=TRUE) {

    posterior.summary <- match.arg(posterior.summary)


    # names of estimated theta
    names.theta <- names(theta)

    # time sequence (must include initial time)
    times <- c(0,data$time)

    message("Compute posterior fit")

    if(posterior.summary=="median"){

        theta.median <- apply(trace[names.theta],2,median)
        traj <- simulateModelReplicates(fitmodel=fitmodel,theta=theta.median,times=times,n=sample.size,observation=TRUE)

    } else if(posterior.summary=="mean"){

        theta.mean <- apply(trace[names.theta],2,mean)
        traj <- simulateModelReplicates(fitmodel=fitmodel,theta=theta.mean,times=times,n=sample.size,observation=TRUE)

    } else if(posterior.summary=="max"){
        ind <- which.max(trace$log.posterior)
        theta.max <- trace[ind,names.theta]
        traj <- simulateModelReplicates(fitmodel=fitmodel,theta=theta.mean,times=times,n=sample.size,observation=TRUE)

    } else {

        sample.size <- min(c(sample.size,nrow(trace)))

        index <- sample(1:nrow(trace), sample.size, replace=TRUE)
        names(index) <- index

        traj <- ldply(index,function(ind) {

            # extract posterior parameter set
            theta <- trace[ind,names.theta]

            # simulate model at successive observation times of data
            traj <- fitmodel$simulate.model(theta,fitmodel$initialise.state(theta),times)

            # generate observation
            traj <- fitmodel$generate.observation(traj,theta)

            return(traj)
        },.progress="text",.id="replicate")
    }


    if(only.fit){
        state.names <- c("observation")
    } else {
        state.names <- NULL
    }

    p <- plotTraj(traj=traj, state.names=state.names, data=data, summary=summary, alpha=alpha, plot=FALSE)


    if(plot){
        print(p)        
    } else {
        return(list(posterior.traj=fit,plot=p))        
    }


}







