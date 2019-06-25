context("mcmc")

test_that("mcmcMH for deterministic SEITL model",{


	theta <- c("R0"=10, "D_lat"=2 , "D_inf"=3, "alpha"=0.5, "D_imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	target <- function(theta) {
		return(logPosterior(fitmodel=SEITL_deter, theta=theta, init.state=init.state, data=data, margLogLike=dTrajObs))
	}

	# default covariance matrix
	suppressMessages(ans <- mcmcMH(target=target, init.theta=theta, n.iterations=100, adapt.size.start=10, adapt.size.cooling=0.99, adapt.shape.start=10, print.info.every=NULL))

	expect_true(is.matrix(ans$trace))
	expect_true(is.numeric(ans$acceptance.rate))
	expect_true(is.matrix(ans$covmat.empirical))

})

test_that("mcmcMH for deterministic SEIT2L model",{


	theta <- c("R0"=10, "D_lat"=2 , "D_inf"=3, "alpha"=0.5, "D_imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T1"=0,"T2"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	target <- function(theta) {
		return(logPosterior(fitmodel=SEIT2L_deter, theta=theta, init.state=init.state, data=data, margLogLike=dTrajObs))
	}

	# default covariance matrix
	suppressMessages(ans <- mcmcMH(target=target, init.theta=theta, n.iterations=100, adapt.size.start=10, adapt.size.cooling=0.99, adapt.shape.start=10, print.info.every=NULL))

	expect_true(is.matrix(ans$trace))
	expect_true(is.numeric(ans$acceptance.rate))
	expect_true(is.matrix(ans$covmat.empirical))

})


test_that("mcmcMH for stochastic SEITL model",{

	theta <- c("R0"=10, "D_lat"=2 , "D_inf"=3, "alpha"=0.5, "D_imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	target <- function(theta) {
		return(logPosterior(fitmodel=SEITL_stoch, theta=theta, init.state=init.state, data=data, margLogLike=margLogLikeSto, n.particles=10))
	}

	# default covariance matrix
	suppressMessages(ans <- mcmcMH(target=target, init.theta=theta, n.iterations=100, adapt.size.start=10, adapt.size.cooling=0.99, adapt.shape.start=10, print.info.every=NULL))

	expect_true(is.matrix(ans$trace))
	expect_true(is.numeric(ans$acceptance.rate))
	expect_true(is.matrix(ans$covmat.empirical))


})


test_that("mcmcMH for stochastic SEIT2L model",{

  theta <- c("R0"=10, "D_lat"=2 , "D_inf"=3, "alpha"=0.5, "D_imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T1"=0,"T2"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	target <- function(theta) {
		return(logPosterior(fitmodel=SEIT2L_stoch, theta=theta, init.state=init.state, data=data, margLogLike=margLogLikeSto, n.particles=10))
	}

	# default covariance matrix
	suppressMessages(ans <- mcmcMH(target=target, init.theta=theta, n.iterations=100, adapt.size.start=10, adapt.size.cooling=0.99, adapt.shape.start=10, print.info.every=NULL))

	expect_true(is.matrix(ans$trace))
	expect_true(is.numeric(ans$acceptance.rate))
	expect_true(is.matrix(ans$covmat.empirical))


})


# test_that("mcmcMH ABC for deterministic SEIT2L model",{

# 	SEIT2L <- SEIT2L_createFitmodel(deterministic=TRUE, verbose=FALSE)

# 	init.theta <- SEIT2L$theta

# 	suppressMessages(ans <- mcmcMH(target=targetPosteriorABC, target.args=list(fitmodel=SEIT2L,epsilon=1), init.theta=init.theta, gaussian.proposal=SEIT2L$gaussian.proposal, n.iterations=100, adapt.size.start=10, adapt.size.cooling=0.99, adapt.shape.start=10, print.info.every=NULL))

# 	expect_true(is.data.frame(ans$trace))
# 	expect_true(is.numeric(ans$acceptance.rate))
# 	expect_true(is.matrix(ans$covmat.empirical))

# })
