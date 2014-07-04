context("logLike")

test_that("log-like deter of SEITL",{

	SEITL <- SEITL_createFitmodel("deterministic")

	theta <- c("R0"=10, "D.lat"=2 , "D.inf"=3, "alpha"=0.5, "D.imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	x <- trajLogLike(fitmodel=SEITL, theta=theta, init.state=init.state, data=data)
	expect_true(is.numeric(x))

})


test_that("log-like deter of SEIT2L",{

	SEIT2L <- SEIT2L_createFitmodel("deterministic")

	theta <- c("R0"=10, "D.lat"=2 , "D.inf"=3, "alpha"=0.5, "D.imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T1"=0,"T2"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	x <- trajLogLike(fitmodel=SEIT2L, theta=theta, init.state=init.state, data=data)
	expect_true(is.numeric(x))


})

test_that("log-like sto of SEITL",{

	SEITL <- SEITL_createFitmodel("stochastic")

	theta <- c("R0"=10, "D.lat"=2 , "D.inf"=3, "alpha"=0.5, "D.imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	x <- margLogLikeSto(fitmodel=SEITL, theta=theta, init.state=init.state, data=data, n.particles=10)
	expect_true(is.numeric(x))
	
})


test_that("log-like sto of SEIT2L",{

	SEIT2L <- SEIT2L_createFitmodel("stochastic")

	theta <- c("R0"=10, "D.lat"=2 , "D.inf"=3, "alpha"=0.5, "D.imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T1"=0,"T2"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]
	
	x <- margLogLikeSto(fitmodel=SEIT2L, theta=theta, init.state=init.state, data=data, n.particles=10)
	expect_true(is.numeric(x))
	
	
})

test_that("posterior deter of SEITL",{

	SEITL <- SEITL_createFitmodel("deterministic")

	theta <- c("R0"=10, "D.lat"=2 , "D.inf"=3, "alpha"=0.5, "D.imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	x <- logPosterior(fitmodel=SEITL, theta=theta, init.state=init.state, data=data, margLogLike=trajLogLike)
	expect_true(all(names(x)==c("log.density","trace")))
	expect_true(is.numeric(x$log.density))

})


test_that("posterior deter of SEIT2L",{

	SEIT2L <- SEIT2L_createFitmodel("deterministic")

	theta <- c("R0"=10, "D.lat"=2 , "D.inf"=3, "alpha"=0.5, "D.imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T1"=0,"T2"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	x <- logPosterior(fitmodel=SEIT2L, theta=theta, init.state=init.state, data=data, margLogLike=trajLogLike)
	expect_true(all(names(x)==c("log.density","trace")))
	expect_true(is.numeric(x$log.density))

})


test_that("posterior sto of SEITL",{

	SEITL <- SEITL_createFitmodel("stochastic")

	theta <- c("R0"=10, "D.lat"=2 , "D.inf"=3, "alpha"=0.5, "D.imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	x <- logPosterior(fitmodel=SEITL, theta=theta, init.state=init.state, data=data, margLogLike=margLogLikeSto, n.particles=10)
	expect_true(all(names(x)==c("log.density","trace")))
	expect_true(is.numeric(x$log.density))

})


test_that("posterior sto of SEIT2L",{

	SEIT2L <- SEIT2L_createFitmodel("stochastic")

	theta <- c("R0"=10, "D.lat"=2 , "D.inf"=3, "alpha"=0.5, "D.imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T1"=0,"T2"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	x <- logPosterior(fitmodel=SEIT2L, theta=theta, init.state=init.state, data=data, margLogLike=margLogLikeSto, n.particles=10)
	expect_true(all(names(x)==c("log.density","trace")))
	expect_true(is.numeric(x$log.density))


})


