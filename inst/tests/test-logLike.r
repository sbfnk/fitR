context("logLike")

test_that("log-like deter of SEITL",{

	theta <- c("R0"=10, "D_lat"=2 , "D_inf"=3, "alpha"=0.5, "D_imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	x <- dTrajObs(fitmodel=SEITL_deter, theta=theta, init.state=init.state, data=data)
	expect_true(is.numeric(x))

})


test_that("log-like deter of SEIT2L",{

	theta <- c("R0"=10, "D_lat"=2 , "D_inf"=3, "alpha"=0.5, "D_imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T1"=0,"T2"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	x <- dTrajObs(fitmodel=SEIT2L_deter, theta=theta, init.state=init.state, data=data)
	expect_true(is.numeric(x))


})

test_that("log-like sto of SEITL",{

	theta <- c("R0"=10, "D_lat"=2 , "D_inf"=3, "alpha"=0.5, "D_imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	x <- margLogLikeSto(fitmodel=SEITL_stoch, theta=theta, init.state=init.state, data=data, n.particles=10)
	expect_true(is.numeric(x))
	
})


test_that("log-like sto of SEIT2L",{

	theta <- c("R0"=10, "D_lat"=2 , "D_inf"=3, "alpha"=0.5, "D_imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T1"=0,"T2"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]
	
	x <- margLogLikeSto(fitmodel=SEIT2L_stoch, theta=theta, init.state=init.state, data=data, n.particles=10)
	expect_true(is.numeric(x))
	
	
})

test_that("posterior deter of SEITL",{

	theta <- c("R0"=10, "D_lat"=2 , "D_inf"=3, "alpha"=0.5, "D_imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	x <- logPosterior(fitmodel=SEITL_deter, theta=theta, init.state=init.state, data=data, margLogLike=dTrajObs)
	expect_true(is.numeric(x))

})


test_that("posterior deter of SEIT2L",{

	theta <- c("R0"=10, "D_lat"=2 , "D_inf"=3, "alpha"=0.5, "D_imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T1"=0,"T2"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	x <- logPosterior(fitmodel=SEIT2L_stoch, theta=theta, init.state=init.state, data=data, margLogLike=dTrajObs)
	expect_true(is.numeric(x))

})


test_that("posterior sto of SEITL",{

	theta <- c("R0"=10, "D_lat"=2 , "D_inf"=3, "alpha"=0.5, "D_imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	x <- logPosterior(fitmodel=SEITL_stoch, theta=theta, init.state=init.state, data=data, margLogLike=margLogLikeSto, n.particles=10)
  expect_true(is.numeric(x))

})


test_that("posterior sto of SEIT2L",{

	theta <- c("R0"=10, "D_lat"=2 , "D_inf"=3, "alpha"=0.5, "D_imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T1"=0,"T2"=0,"L"=4,"Inc"=0)
	data("FluTdC1971",envir = environment())
	data <- FluTdC1971[1:5,]

	x <- logPosterior(fitmodel=SEIT2L_stoch, theta=theta, init.state=init.state, data=data, margLogLike=margLogLikeSto, n.particles=10)
  expect_true(is.numeric(x))


})


