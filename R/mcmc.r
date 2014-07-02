#'Metropolis-Hasting MCMC
#'
#'Run \code{n.iterations} of a Metropolis-Hasting MCMC to sample from the target distribution using a gaussian proposal kernel.
#'Two optional optimizations are also implemented: truncated gaussian proposal (to match the support of the target distribution, i.e. boundary of the parameters) and adaptative gaussian proposal (to match the size and the shape of the target distribution).
#' @param target \R-function that takes a single argument: \code{theta} (named numeric vector of parameter values) and returns a list of 2 elements:
#' \itemize{
#' \item \code{log.density} the logged value of the target density, evaluated at \code{theta}.
#' \item \code{trace} a named numeric vector of values to be printed in the \code{trace} data.frame returned by \code{mcmcMH}.
#' }
#' @param theta.init named vector of initial parameter values to start the chain.
#' @param proposal.sd vector of standard deviations. If this is given and covmat is not, a diagonal matrix will be built from this to use as covariance matrix of the multivariate Gaussian proposal distribution. By default, this is set to \code{theta.init/10}.
#' @param covmat named numeric covariance matrix of the multivariate Gaussian proposal distribution. Must have named rows and columns with at least all estimated theta. If \code{proposal.sd} is given, this is ignored.
#' @param n.iterations number of iterations to run the MCMC chain.
#' @param limits limits for the - potentially truncated - multi-variate normal proposal distribution of the MCMC. Contains 2 elements:
#' \itemize{
#'	\item \code{lower} named numeric vector. Lower truncation points in each dimension of the Gaussian proposal distribution. By default they are set to \code{-Inf}.
#'	\item \code{upper} named numeric vector. Upper truncation points in each dimension of the Gaussian proposal distribution. By default they are set to \code{Inf}.
#' }
#' @param adapt.size.start number of iterations to run before adapting the size of the proposal covariance matrix (see note below). Set to 0 (default) if size is not to be adapted.
#' @param adapt.size.cooling cooling factor for the scaling factor of the covariance matrix during size adaptation (see note below).
#' @param adapt.shape.start number of accepted jumps before adapting the shape of the proposal covariance matrix (see note below). Set to 0 (default) if shape is not to be adapted
#' @param print.info.every frequency of information on the chain: acceptance rate and state of the chain. Default value to \code{n.iterations/100}. Set to \code{NULL} to avoid any info.
#' @note The size of the proposal covariance matrix is adapted using the following formulae: \deqn{\Sigma_{n+1}=\sigma_n * \Sigma_n} with \eqn{\sigma_n=\sigma_{n-1}*exp(\alpha^n*(acc - 0.234))},
#' where \eqn{\alpha} is equal to \code{adapt.size.cooling} and \eqn{acc} is the acceptance rate of the chain.
#'
#' The shape of the proposal covariance matrix is adapted using the following formulae: \deqn{\Sigma_{n+1}=2.38^2/d * \Sigma_n} with \eqn{\Sigma_n} the empirical covariance matrix
#' and \eqn{d} is the number of estimated parameters in the model.
#' @references Roberts GO, Rosenthal JS. Examples of adaptive MCMC. Journal of Computational and Graphical Statistics. Taylor & Francis; 2009;18(2):349-67.
#' @export
#' @import tmvtnorm 
#' @importFrom lubridate as.period
#' @return a list with 3 elements:
#'\itemize{
#'	\item \code{trace} a \code{data.frame}. Each row contains a state of the chain (as returned by \code{target}).
#'	\item \code{acceptance.rate} acceptance rate of the MCMC chain.
#'	\item \code{covmat.empirical} empirical covariance matrix of the target sample.
#'}
mcmcMH <- function(target, theta.init, proposal.sd = NULL, n.iterations, covmat = NULL, limits=list(lower = NULL, upper = NULL), adapt.size.start = NULL, adapt.size.cooling = 0.99, adapt.shape.start = NULL, print.info.every = n.iterations/100) {

	# initialise theta
	theta.current <- theta.init
	theta.propose <- theta.init

	# extract theta of gaussian proposal
	covmat.proposal <- covmat
	lower.proposal <- limits$lower
	upper.proposal <- limits$upper

	# reorder vector and matrix by names, set to default if necessary 
	theta.names <- names(theta.init)
	if(is.null(covmat.proposal)){
                if (is.null(proposal.sd)){
                        proposal.sd <- rep(theta.init/10, nrow=length(theta.names))
                }
                covmat.proposal <- matrix(diag(proposal.sd^2),nrow=length(theta.names),dimnames=list(theta.names,theta.names))
	} else {
		covmat.proposal <- covmat.proposal[theta.names,theta.names]		
	}
	if(is.null(lower.proposal)){
		lower.proposal <- theta.init
		lower.proposal[] <- -Inf
	} else {
		lower.proposal <- lower.proposal[theta.names]		
	}
	if(is.null(upper.proposal)){
		upper.proposal <- theta.init
		upper.proposal[] <- Inf
	} else {
		upper.proposal <- upper.proposal[theta.names]		
	}

	# covmat init
	covmat.proposal.init <- covmat.proposal
	start.adapt.size <- (adapt.size.start > 0)
	start.adapt.shape <- (adapt.shape.start > 0)

	# find estimated theta
	theta.estimated.names <- names(which(diag(covmat.proposal)>0))

	# evaluate target at theta init
	target.theta.current <- target(theta.current)

        # if return value is a vector, set log.density and trace
        if (class(target.theta.current) == "numeric") {
                suppressWarnings(target.theta.current$log.density <- target.theta.current)
                suppressWarnings(target.theta.current$trace <- theta.current)
        }

        trace <- data.frame(t(target.theta.current[["trace"]]))

	# acceptance rate
	acceptance.rate <- 0

	# scaling factor for covmat size
	scaling.sd  <- 1

        if (adapt.size.start > 0 || adapt.shape.start > 0) {
	        # empirical covariance matrix (0 everywhere initially)
                covmat.empirical <- covmat.proposal
                covmat.empirical[,] <- 0

	        # empirical mean vector
                theta.mean <- theta.current
        }

	# if print.info.every is null never print info
	if(is.null(print.info.every)){
		print.info.every <- n.iterations + 1
	}

	start_iteration_time <- Sys.time()

	for (i.iteration in seq_len(n.iterations)) {

		# adaptive step
		if(adapt.size.start > 0 && i.iteration >= adapt.size.start && acceptance.rate*i.iteration < adapt.shape.start){
			if(start.adapt.size){
				message("\n---> Start adapting size of covariance matrix")
				start.adapt.size <- 0				
			}
			# adapt size of covmat until we get enough accepted jumps
			scaling.sd <- scaling.sd*exp(adapt.size.cooling^(i.iteration-adapt.size.start)*(acceptance.rate - 0.234))
			covmat.proposal <- scaling.sd^2*covmat.proposal.init

		}else if(adapt.shape.start > 0 && acceptance.rate*i.iteration >= adapt.shape.start){
			if(start.adapt.shape){
				message("\n---> Start adapting shape of covariance matrix")
				# flush.console()
				start.adapt.shape <- 0
			}
			# adapt shape of covmat using optimal scaling factor for multivariate traget distributions
			covmat.proposal <- 2.38^2/length(theta.estimated.names)*covmat.empirical
		}

		# print info
		if(i.iteration%%round(print.info.every)==0){
			## end_iteration_time <- Sys.time()
			state.mcmc <- trace[nrow(trace),]
			## suppressMessages(time.estimation <- round(as.period((end_iteration_time-start_iteration_time)*10000/round(print.info.every))))
			## message("Iteration: ",i.iteration,"/",n.iterations,", ETA: ",time.estimation,", acceptance rate: ",sprintf("%.3f",acceptance.rate),appendLF=FALSE)
			message("Iteration: ",i.iteration,"/",n.iterations,", acceptance rate: ",sprintf("%.3f",acceptance.rate),appendLF=FALSE)
                        if (adapt.size.start > 0 || adapt.shape.start > 0) {
                                message(", scaling.sd: ",sprintf("%.3f",scaling.sd),appendLF=FALSE)
                        }
                        message(", state: ",printNamedVector(state.mcmc))
			## start_iteration_time <- end_iteration_time
		}

		# propose another parameter set
		if(any(diag(covmat.proposal)[theta.estimated.names]<.Machine$double.eps)){
			print(covmat.proposal[theta.estimated.names,theta.estimated.names])
			stop("non-positive definite covmat",call.=FALSE)
		}
		theta.propose[theta.estimated.names] <- as.vector(rtmvnorm(1,mean=theta.current[theta.estimated.names],sigma=covmat.proposal[theta.estimated.names,theta.estimated.names],lower=lower.proposal[theta.estimated.names],upper=upper.proposal[theta.estimated.names]))

		# evaluate posterior of proposed parameter
		target.theta.propose <- target(theta.propose)
                # if return value is a vector, set log.density and trace
                if (class(target.theta.propose) == "numeric") {
                        suppressWarnings(target.theta.propose$log.density <- target.theta.propose)
                        suppressWarnings(target.theta.propose$trace <- theta.propose)
                }

		if(!is.finite(target.theta.propose$log.density)){
			# if posterior is 0 then do not compute anything else and don't accept
			log.acceptance <- -Inf

		}else{

			# compute Metropolis-Hastings ratio (acceptance probability)
			log.acceptance <- target.theta.propose$log.density - target.theta.current$log.density + dtmvnorm(x=theta.current[theta.estimated.names],mean=theta.propose[theta.estimated.names],sigma=covmat.proposal[theta.estimated.names,theta.estimated.names],lower=lower.proposal[theta.estimated.names],upper=upper.proposal[theta.estimated.names],log=TRUE) - dtmvnorm(x=theta.propose[theta.estimated.names],mean=theta.current[theta.estimated.names],sigma=covmat.proposal[theta.estimated.names,theta.estimated.names],lower=lower.proposal[theta.estimated.names],upper=upper.proposal[theta.estimated.names],log=TRUE)

		}

		if(is.accepted <- (log(runif(1)) < log.acceptance)){
                    # accept proposed parameter set
                    theta.current <- theta.propose
                    target.theta.current <- target.theta.propose
                }
                trace <- rbind(trace,c(target.theta.current$trace))

		# update acceptance rate
		acceptance.rate <- acceptance.rate + (is.accepted - acceptance.rate)/i.iteration

		# update empirical covariance matrix
                if (adapt.size.start > 0 || adapt.shape.start > 0) {
                        tmp <- updateCovmat(covmat.empirical,theta.mean,theta.current,i.iteration)
                        covmat.empirical <- tmp$covmat
                        theta.mean <- tmp$theta.mean
                }

	}

        if (start.adapt.shape){
                return(list(trace=trace,acceptance.rate=acceptance.rate,covmat.empirical=covmat.empirical))
        } else {
                return(list(trace=trace,acceptance.rate=acceptance.rate))
        }
}



