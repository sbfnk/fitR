context("mcmc")

test_that("mcmcMH for deterministic SEITL model",{


	fitmodel <- SEITL_createFitmodel("deterministic")

	theta <- c("R0"=10, "D.lat"=2 , "D.inf"=3, "alpha"=0.5, "D.imm"=15, "rho"=0.7)
	state.init <- c("S"=280,"E"=0,"I"=2,"T"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	target <- function(theta) {
		return(logPosterior(fitmodel=fitmodel, theta=theta, state.init=state.init, data=data, margLogLike=trajLogLike))
	}

	# default covariance matrix
	suppressMessages(ans <- mcmcMH(target=target, theta.init=theta, n.iterations=100, adapt.size.start=10, adapt.size.cooling=0.99, adapt.shape.start=10, print.info.every=NULL))

	expect_true(is.data.frame(ans$trace))
	expect_true(is.numeric(ans$acceptance.rate))
	expect_true(is.matrix(ans$covmat.empirical))

})

test_that("mcmcMH for deterministic SEIT2L model",{


	fitmodel <- SEIT2L_createFitmodel("deterministic")

	theta <- c("R0"=10, "D.lat"=2 , "D.inf"=3, "alpha"=0.5, "D.imm"=15, "rho"=0.7)
	state.init <- c("S"=280,"E"=0,"I"=2,"T1"=0,"T2"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	target <- function(theta) {
		return(logPosterior(fitmodel=fitmodel, theta=theta, state.init=state.init, data=data, margLogLike=trajLogLike))
	}

	# default covariance matrix
	suppressMessages(ans <- mcmcMH(target=target, theta.init=theta, n.iterations=100, adapt.size.start=10, adapt.size.cooling=0.99, adapt.shape.start=10, print.info.every=NULL))

	expect_true(is.data.frame(ans$trace))
	expect_true(is.numeric(ans$acceptance.rate))
	expect_true(is.matrix(ans$covmat.empirical))

})


test_that("mcmcMH for stochastic SEITL model",{

	fitmodel <- SEITL_createFitmodel("stochastic")

	theta <- c("R0"=10, "D.lat"=2 , "D.inf"=3, "alpha"=0.5, "D.imm"=15, "rho"=0.7)
	state.init <- c("S"=280,"E"=0,"I"=2,"T"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	target <- function(theta) {
		return(logPosterior(fitmodel=fitmodel, theta=theta, state.init=state.init, data=data, margLogLike=margLogLikeSto, n.particles=10))
	}

	# default covariance matrix
	suppressMessages(ans <- mcmcMH(target=target, theta.init=theta, n.iterations=100, adapt.size.start=10, adapt.size.cooling=0.99, adapt.shape.start=10, print.info.every=NULL))

	expect_true(is.data.frame(ans$trace))
	expect_true(is.numeric(ans$acceptance.rate))
	expect_true(is.matrix(ans$covmat.empirical))


})


test_that("mcmcMH for stochastic SEIT2L model",{

	fitmodel <- SEIT2L_createFitmodel("stochastic")

	theta <- c("R0"=10, "D.lat"=2 , "D.inf"=3, "alpha"=0.5, "D.imm"=15, "rho"=0.7)
	state.init <- c("S"=280,"E"=0,"I"=2,"T1"=0,"T2"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	target <- function(theta) {
		return(logPosterior(fitmodel=fitmodel, theta=theta, state.init=state.init, data=data, margLogLike=margLogLikeSto, n.particles=10))
	}

	# default covariance matrix
	suppressMessages(ans <- mcmcMH(target=target, theta.init=theta, n.iterations=100, adapt.size.start=10, adapt.size.cooling=0.99, adapt.shape.start=10, print.info.every=NULL))

	expect_true(is.data.frame(ans$trace))
	expect_true(is.numeric(ans$acceptance.rate))
	expect_true(is.matrix(ans$covmat.empirical))


})


# test_that("mcmcMH ABC for deterministic SEIT2L model",{

# 	SEIT2L <- SEIT2L_createFitmodel(deterministic=TRUE, verbose=FALSE)

# 	theta.init <- SEIT2L$theta

# 	suppressMessages(ans <- mcmcMH(target=targetPosteriorABC, target.args=list(fitmodel=SEIT2L,epsilon=1), theta.init=theta.init, gaussian.proposal=SEIT2L$gaussian.proposal, n.iterations=100, adapt.size.start=10, adapt.size.cooling=0.99, adapt.shape.start=10, print.info.every=NULL))

# 	expect_true(is.data.frame(ans$trace))
# 	expect_true(is.numeric(ans$acceptance.rate))
# 	expect_true(is.matrix(ans$covmat.empirical))

# })
