#'Plot model trajectories
#'
#'This function use faceting to plot all trajectories in a data frame. Convenient to see results of several simulations, or data. Also, if \code{data} is present, then an additional plot is displayed with data and potentially observation generated.
#' @param traj data.frame, output of \code{fitmodel$simulate} or \code{simulateModelReplicates}.
#' @param state.names character vector. Names of the state variables to plot. Names must match \code{fitmodel$state.names}. If \code{NULL} (default) all state variables are plotted.
#' @param data data frame. Observation times and observed data. The time column must be named \code{time}, whereas the name of the data column should match one of \code{fitmodel$state.names}.
#' @param summary logical. If \code{TRUE}, the mean, median as well as the 50th and 95th percentile of the trajectories are plotted (default). If \code{FALSE}, all individual trajectories are plotted (transparency can be set with \code{alpha}).
#' @param non.extinct character vector. Names of the infected states which must be non-zero so the epidemic is still ongoing. 
#' When the names of these states are provided, the extinction probability is plotted by computing the proportion of faded-out epidemics over time. 
#' An epidemic has faded-out when all the infected states (whose names are provided) are equal to 0. This is only relevant for stochastic models. 
#' In addition, if \code{summary==TRUE}, the summaries of the trajectories conditioned on non-extinction are shown. Default to \code{NULL}.
#' @param alpha transparency of the trajectories (between 0 and 1).
#' @param plot if \code{TRUE} the plot is displayed, and returned otherwise.
#' @param init_date character. Date of the first point of the time series (default to \code{NULL}). If provided, the x-axis will be in calendar format. NB: currently only works if the unit of time is the day.
#' @export
#' @import reshape2 ggplot2 stringr
#' @seealso \code{\link{simulateModelReplicates}}
plotTraj <- function(traj=NULL, state.names=NULL, data=NULL, summary=TRUE, non.extinct=NULL, alpha=1, plot=TRUE, init_date=NULL) {

    if(!is.null(init_date)){
        init_date <- as.Date(init_date)
    }

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

    if(!is.null(init_date)){
        traj <- mutate(traj, time = init_date + time)
        if(!is.null(data)){
            data <- mutate(data, time = init_date + time)            
        }    
    }

    if (!is.null(traj)) {



        if(!is.null(non.extinct)){
            traj <- mutate(traj,infected=eval(parse(text=paste(non.extinct,collapse="+")),traj))
            df.infected <- melt(traj,measure.vars="infected",variable.name="state")
            df.p.ext <- ddply(df.infected,"time",function(df){
                return(data.frame(value=sum(df$value==0)/nrow(df)))
            })
            df.p.ext$state <- "p.extinction"
            df.p.ext$replicate <- 0

            if(summary){
                traj <- subset(traj, infected>0)
                traj$infected <- NULL
            }
        }
        
        df.traj <- melt(traj,measure.vars=state.names,variable.name="state")
        df.traj <- subset(df.traj, !is.na(value))


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

        if(!is.null(non.extinct)){
            p <- p+geom_line(data=df.p.ext,aes(x=time,y=value),color="black",alpha=1)
        }

    } else {
        p <- ggplot()
    }

    if(!is.null(data)){
        obs_names <- grep("obs",names(data),value=TRUE)
        data <- melt(data, measure.vars=obs_names,variable.name="state")
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
plotFit <- function(fitmodel, theta, init.state, data, n.replicates=1, summary=TRUE, alpha=min(1,10/n.replicates), all.vars=FALSE, non.extinct=NULL, plot=TRUE) {

    times <- c(0, data$time)

    if (n.replicates > 1) {
        cat("Simulate ",n.replicates," replicate(s)\n")
    }
    traj <- simulateModelReplicates(fitmodel=fitmodel,theta=theta, init.state=init.state, times=times, n=n.replicates, observation=TRUE)

    if(all.vars){
        state.names <- NULL
    } else {
        state.names <- grep("obs",names(traj),value=TRUE)
    }

    p <- plotTraj(traj=traj, state.names=state.names, data=data, summary=summary, alpha=alpha, non.extinct=non.extinct, plot=FALSE)

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


        obs <- ddply(df, "time" , fitmodel$genObsPoint, theta = theta)
        traj_obs <- join(df,obs, by="time")

        return(traj_obs)

    },.id="replicate")

    if(all.vars){
        state.names <- NULL
    } else {
        state.names <- grep("obs",names(traj),value=TRUE)
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
#'Plot the posterior density.
#' @param trace either a \code{data.frame} or a \code{list} of \code{data.frame} with all variables in column, as outputed by \code{\link{mcmcMH}}. Accept also an \code{mcmc}, a \code{mcmc.list} object or a \code{list} of \code{mcmc.list} .
#' @param prior a \code{data.frame} containing the prior density. It must have the three following columns: 
#' \itemize{
#'     \item \code{theta} names of the parameters
#'     \item \code{x} value of the parameters
#'     \item \code{density} density of the prior at \code{x} 
#' }
#' @param colour named vector of two characters and containing colour names for posterior and prior distributions. Vector names must be \code{posterior} and \code{prior}.
#' @inheritParams plotTraj
#' @export
#' @import ggplot2 reshape2
#' @seealso burnAndThin
plotPosteriorDensity <- function(trace, prior=NULL, colour=NULL, plot=TRUE){

    if(is.null(colour)){
        colour <- c(posterior="#7570b3",prior="#d95f02")
    }

    if(class(trace)%in%c("mcmc.list","list")){

        if(all(sapply(trace,function(x) {class(x)=="mcmc.list"}))){
            trace <- llply(trace,function(x) {names(x) <- NULL;ldply(x)})
        }

        if(is.null(names(trace))){
            names(trace) <- seq_along(trace)            
        }
        trace <- ldply(trace,.id="chain")
    } else {
        trace$chain <- 1
    }

    df_posterior <- melt(trace,id.vars="chain", value.name="x", variable.name="theta") 

    p <- ggplot(df_posterior,aes(x=x))+facet_wrap(~theta,scales="free")

    if(n_distinct(df_posterior$chain)>1){
        p <- p + geom_density(data=df_posterior,aes(y=..density.., colour=chain))
    } else {
        p <- p + geom_histogram(data=df_posterior,aes(y=..density..), fill=colour[["posterior"]], colour=colour[["posterior"]],alpha=0.5)
        # p <- p + geom_density(aes(y=..density..),fill="black",alpha=0.5)
    }

    if(!is.null(prior)){
        p <- p + geom_area(data=prior,aes(x=x,y=density),fill=colour[["prior"]],alpha=0.5)
    }

    p <- p + theme_bw() + xlab("value")
    
    if(plot){
        print(p)        
    } else {
        return(p)
    }

}


#'2D highest posterior density region
#'
#'Given a sample from a multivariate posterior distribution, plot the bivariate region of highest marginal posterior density (HPD) for two variables with defined levels.
#' @param trace either a \code{data.frame} or \code{mcmc} object.
#' @inheritParams plotPosteriorDensity
#' @inheritParams emdbook::HPDregionplot
#' @note HPD levels are computed using the function \code{\link[emdbook]{HPDregionplot}} from the package \code{emdbook}.
#' @export
#' @import ggplot2
#' @importFrom emdbook HPDregionplot
plotHPDregion2D <- function(trace, vars, prob=c(0.95,0.75,0.5,0.25,0.1), xlab=NULL, ylab=NULL, plot=TRUE) {

    if(length(vars)!=2){
        stop(sQuote("vars")," is not a vector of length 2",call.=FALSE)
    }

    list_HPD <- HPDregionplot(trace,vars=vars,prob=prob,n=100)
    levels_HPD <- unique(sapply(list_HPD,function(x){x$level}))
    names(levels_HPD) <- paste0(prob*100,"%")

    p <- ggplot(trace, aes_string(x=vars[1],y=vars[2]))
    p <- p + stat_density2d(aes(alpha=..level..),fill="red",colour="black", geom="polygon",breaks=levels_HPD) 
    p <- p + scale_alpha("HPD", breaks=levels_HPD,guide="legend", range=c(0.1,0.45))
    if(!is.null(xlab)){
        p <- p + xlab(xlab)         
    }
    if(!is.null(ylab)){
        p <- p + ylab(ylab)     
    }

    p <- p + theme_bw() + guides(alpha=guide_legend(override.aes=list(colour=NA,alpha=seq(0.1,0.9,length=length(levels_HPD)))))
    
    if(plot){
        print(p)        
    }

    invisible(p)
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
#' @inheritParams plotFit
#' @export
#' @import ggplot2 plyr
#' @return If \code{plot==FALSE}, a list of 2 elements is returned:
#'\itemize{
#'    \item \code{theta} the \code{theta}(s) used for plotting (either a \code{vector} or a \code{data.frame})
#'    \item \code{traj} a \code{data.frame} with the trajectories (and observations) sampled from the posterior distribution.
#'    \item \code{plot} the plot of the fit displayed.
#'}
plotPosteriorFit <- function(trace, fitmodel, init.state, data, posterior.summary=c("sample","median","mean","max"), summary=TRUE, sample.size = 100, non.extinct=NULL, alpha=min(1,10/sample.size), plot=TRUE, all.vars = FALSE, init_date=NULL) {

    posterior.summary <- match.arg(posterior.summary)

    if(class(trace)=="mcmc"){
        trace <- as.data.frame(trace)
    } else if (class(trace)=="mcmc.list"){
        trace <- ldply(trace)
    }

    # names of estimated theta
    theta.names <- fitmodel$theta.names

    # time sequence (must include initial time)
    times <- c(0,data$time)

    message("Compute posterior fit")

    if(posterior.summary=="median"){

        theta <- apply(trace[theta.names],2,median)
        traj <- simulateModelReplicates(fitmodel=fitmodel,init.state=init.state, theta=theta,times=times,n=sample.size,observation=TRUE)

    } else if(posterior.summary=="mean"){

        theta <- apply(trace[theta.names],2,mean)
        traj <- simulateModelReplicates(fitmodel=fitmodel,init.state=init.state, theta=theta,times=times,n=sample.size,observation=TRUE)

    } else if(posterior.summary=="max"){
        ind <- which.max(trace$log.posterior)
        theta <- trace[ind,theta.names]
        traj <- simulateModelReplicates(fitmodel=fitmodel,init.state=init.state, theta=theta,times=times,n=sample.size,observation=TRUE)

    } else {

        sample.size <- min(c(sample.size,nrow(trace)))

        index <- sample(1:nrow(trace), sample.size, replace=TRUE)
        names(index) <- index

        traj <- ldply(index,function(ind) {

            # extract posterior parameter set
            theta <- trace[ind,theta.names]

            # simulate model at successive observation times of data
            traj <- rObsTraj(fitmodel, theta, init.state, times)

            return(traj)
        },.progress="text", .id="replicate")

        theta <- trace[index,theta.names]
    }


    if(all.vars){
        state.names <- NULL
    } else {
        state.names <- grep("obs",names(traj),value=TRUE)
    }

    traj <- subset(traj, time>0)

    p <- plotTraj(traj=traj, state.names=state.names, data=data, summary=summary, alpha=alpha, non.extinct=non.extinct, plot=FALSE, init_date=init_date)


    if(plot){
        print(p)
    } else {
        return(list(theta=theta,traj=traj,plot=p))
    }


}

##' Plot Effective Sample Size (ESS) against burn-in
##'
##' Takes an mcmc trace and tests the ESS at different values of burn-in
##' @param trace either a \code{data.frame} or a \code{list} of \code{data.frame} with all variables in column, as outputed by \code{\link{mcmcMH}}. Accept also \code{mcmc} or \code{mcmc.list} objects.
##' @param longest.burn.in The longest burn-in to test. Defaults to half the length of the trace
##' @param step.size The size of the steps of burn-in to test. Defaults to 1/50th of \code{longest.burn.in}
##' @return a plot of the ESS against burn.in
##' @export
##' @import coda ggplot2 reshape2 plyr
plotESSBurn <- function(trace, longest.burn.in = ifelse(is.data.frame(trace) | is.mcmc(trace),nrow(trace),nrow(trace[[1]])) / 2, step.size = round(longest.burn.in / 50)) {

    test.burn.in <- seq(0, longest.burn.in, step.size) # test values

    if(!class(trace)%in%c("mcmc.list","list")){
        trace <- list("chain1"=trace)
        no_color <- TRUE
    } else {
        no_color  <- FALSE
    }

    if(is.null(names(trace))){
        names(trace) <- seq_along(trace)            
    }

    df_ess.burn.in <- ldply(trace, function(one.trace){

# initialise data.frame of ess estimates
        ess.burn.in <- data.frame(t(effectiveSize(one.trace)))
        for (burn.in in test.burn.in[-1]) { 
            # loop over all test values after 0
            # test burn-in
            test.trace <- burnAndThin(one.trace, burn = burn.in)
            # estimate ESS and at to vector of ess estimates
            ess.burn.in <- rbind(ess.burn.in, t(effectiveSize(as.mcmc(test.trace))))
        }
        ess.burn.in$burn.in <- test.burn.in

        return(ess.burn.in)

    }, .id="chain")

    ess.long <- melt(df_ess.burn.in, id.vars = c("chain","burn.in"),value.name = "ESS", variable.name = "parameter")

    p <- ggplot(ess.long, aes(x = burn.in, y = ESS))
    p <- p + facet_wrap(~ parameter)
    if(no_color){
        p <- p + geom_line()
    } else {
        p <- p + geom_line(aes(color=chain))
    }
    p <- p + theme_bw()

    print(p)
}
