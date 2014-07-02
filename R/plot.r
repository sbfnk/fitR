#'Plot model trajectories
#'
#'This function use faceting to plot all trajectories in a data frame. Convenient to see results of several simulations, or data. Also, if \code{data} is present, then an additional plot is displayed with data and potentially observation generated.
#' @param traj data.frame, output of \code{fitmodel$simulate} or \code{simulateModelReplicates}.
#' @param state.names character vector. Names of the state variables to plot. Names must match \code{fitmodel$state.names}. If \code{NULL} (default) all state variables are plotted.
#' @param data data frame. Observation times and observed data. The time column must be named \code{time}, whereas the name of the data column should match one of \code{fitmodel$state.names}.
#' @param summary logical. If \code{TRUE}, the mean, median as well as the 50th and 95th percentile of the trajectories are plotted (default). If \code{FALSE}, all individual trajectories are plotted (transparency can be set with \code{alpha}).
#' @param p.extinction logical. If \code{TRUE}, the time-series of the proportion of faded-out epidemics is plotted (default to \code{FALSE}). This is only relevant for stochastic models.
#' @param alpha transparency of the trajectories (between 0 and 1).
#' @param plot if \code{TRUE} the plot is displayed, and returned otherwise.
#' @export
#' @import reshape2 ggplot2 stringr
#' @seealso \code{\link{simulateModelReplicates}}
plotTraj <- function(traj=NULL, state.names=NULL, data=NULL, summary=TRUE, p.extinction=FALSE, alpha=1, plot=TRUE) {

    if(is.null(traj) && is.null(data)){
        stop("Nothing to plot")
    }

    if(!is.null(traj) & !any(duplicated(traj$time))){
        traj$replicate <- 1

        if(summary){
            # Only 1 replicate to summarise: mean, median and CI of
            # the trajectories won't be plotted.
            summary <- FALSE
        }
    }

    if(is.null(state.names)){
        state.names <- setdiff(names(traj),c("time","replicate"))
    }

    if (!is.null(traj)) {

        df.traj <- melt(traj,measure.vars=state.names,variable.name="state")

        if(p.extinction){
            infected.names <- unlist(sapply(c("E","I"),grep,x=names(traj),value=TRUE))
            df.infected <- mutate(traj,infected=eval(parse(text=paste(infected.names,collapse="+"))))
            df.infected <- melt(df.infected,measure.vars="infected",variable.name="state")
            df.p.ext <- ddply(df.infected,"time",function(df){
                return(data.frame(value=sum(df$value==0)/nrow(df)))
            })
            df.p.ext$state <- "p. extinction"
            df.p.ext$replicate <- 0
        }

        if (summary){

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

        if(p.extinction){
            p <- p+geom_line(data=df.p.ext,aes(x=time,y=value),color="black",alpha=1)
        }

    } else {
        p <- ggplot()
    }

    if(!is.null(data)){

        data <- melt(data, measure.vars="obs",variable.name="state")
        p <- p + geom_point(data=data,aes(x=time,y=value),colour="black")

    }

    p <- p + theme_bw() + theme(legend.position="top", legend.box="horizontal")

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
#' @param all.vars logical, if \code{FALSE} only the observations are plotted. Otherwise, all state variables are plotted.
#' @inheritParams testFitmodel
#' @inheritParams plotTraj
#' @export
#' @import plyr ggplot2
#' @return if \code{plot==FALSE}, a list of 2 elements is returned:
#' \itemize{
#'     \item \code{simulations} \code{data.frame} of \code{n.replicates} simulated observations.
#'     \item \code{plot} the plot of the fit.
#' }
plotFit <- function(fitmodel, theta, state.init, data, n.replicates=1, summary=TRUE, alpha=min(1,10/n.replicates), all.vars=FALSE, p.extinction=FALSE, plot=TRUE) {

    times <- c(0, data$time)

    if (n.replicates > 1) {
        cat("Simulate ",n.replicates," replicate(s)\n")
    }
    traj <- simulateModelReplicates(fitmodel=fitmodel,theta=theta, state.init=state.init, times=times, n=n.replicates, observation=TRUE)

    if(all.vars){
        state.names <- NULL
    } else {
        state.names <- c("obs")
    }

    p <- plotTraj(traj=traj, state.names=state.names, data=data, summary=summary, alpha=alpha, p.extinction=p.extinction, plot=FALSE)

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
plotSMC <- function(smc, fitmodel, theta, data=NULL, summary=TRUE, alpha=1, all.vars=FALSE, plot=TRUE) {

    traj <- smc$traj
    names(traj) <- 1:length(traj)

    traj <- ldply(traj,function(df) {

        df$obs <- apply(X = traj, MARGIN = 1, FUN = fitmodel$genObsPoint, theta = theta)
        return(df)

    },.id="replicate")

    if(all.vars){
        state.names <- NULL
    } else {
        state.names <- c("obs")
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
plotPosteriorFit <- function(trace, fitmodel, state.init, posterior.summary=c("sample","median","mean","max"), summary=TRUE, sample.size = 100, alpha=min(1,10/sample.size), plot=TRUE) {

    posterior.summary <- match.arg(posterior.summary)


    # names of estimated theta
    theta.names <- fitmodel$theta.names

    # time sequence (must include initial time)
    times <- c(0,data$time)

    message("Compute posterior fit")

    if(posterior.summary=="median"){

        theta.median <- apply(trace[theta.names],2,median)
        traj <- simulateModelReplicates(fitmodel=fitmodel,theta=theta.median,times=times,n=sample.size,observation=TRUE)

    } else if(posterior.summary=="mean"){

        theta.mean <- apply(trace[theta.names],2,mean)
        traj <- simulateModelReplicates(fitmodel=fitmodel,theta=theta.mean,times=times,n=sample.size,observation=TRUE)

    } else if(posterior.summary=="max"){
        ind <- which.max(trace$log.posterior)
        theta.max <- trace[ind,theta.names]
        traj <- simulateModelReplicates(fitmodel=fitmodel,theta=theta.mean,times=times,n=sample.size,observation=TRUE)

    } else {

        sample.size <- min(c(sample.size,nrow(trace)))

        index <- sample(1:nrow(trace), sample.size, replace=TRUE)
        names(index) <- index

        traj <- ldply(index,function(ind) {

            # extract posterior parameter set
            theta <- trace[ind,theta.names]

            # simulate model at successive observation times of data
            traj <- genObsTraj(fitmodel, theta, state.init, times)

            return(traj)
        },.progress="text",.id="replicate")
    }


    if(only.fit){
        state.names <- c("obs")
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

##' Plot Effective Sample Size (ESS) against burn-in
##'
##' Takes an mcmc trace and tests the ESS at different values of burn-in
##' @param trace A data frame of an MCMC chain with one column per parameter
##' @param longest.burn.in The longest burn in to test. Defaults to half the length of the trace
##' @param step.size The size of the steps of burn-in to test. Defaults to 1/50th of \code{longest.burn.in}
##' @return a plot of the ESS against burn.in
plotESSBurn <- function(trace, longest.burn.in = nrow(trace) / 2, step.size = round(longest.burn.in / 50)) {

        test.burn.in <- seq(0, longest.burn.in, step.size) # test values
        # initialise data.frame of ess estimates
        ess.burn.in <- data.frame(t(effectiveSize(trace)))
        for (burn.in in test.burn.in[-1]) { # loop over all test values after 0
                # test burn-in
                test.trace <- burnAndThin(trace, burn = burn.in)
                # estimate ESS and at to vector of ess estimates
                ess.burn.in <- rbind(ess.burn.in, t(effectiveSize(mcmc(test.trace))))
        }
        ess.burn.in$burn.in <- test.burn.in
        ess.long <- melt(ess.burn.in, id.vars = c("burn.in"),
                         value.name = "ESS", variable.name = "parameter")
        p <- ggplot(ess.long, aes(x = burn.in, y = ESS))+ facet_wrap(~ parameter)+ geom_line()

        print(p)
}
