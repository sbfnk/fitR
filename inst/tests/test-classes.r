context("classes")

test_that("constructor for SEITL model",{

# load and rename data
	
	SEITL_det <- SEITL_createFitmodel(simulate="deterministic")
	expect_true(inherits(SEITL_det,"fitmodel"))

	SEITL_stoch <- SEITL_createFitmodel(simulate="stochastic")
	expect_true(inherits(SEITL_stoch,"fitmodel"))

	SEIT2L_det <- SEIT2L_createFitmodel(simulate="deterministic")
	expect_true(inherits(SEIT2L_det,"fitmodel"))

	SEIT2L_stoch <- SEIT2L_createFitmodel(simulate="stochastic")
	expect_true(inherits(SEIT2L_stoch,"fitmodel"))


	## test them
	theta <- c("R0"=10, "D.lat"=2 , "D.inf"=3, "alpha"=0.5, "D.imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T"=0,"L"=4,"Inc"=0)

	data("FluTdC1971",envir = environment())

	testFitmodel(fitmodel=SEITL_det, theta=theta, init.state=init.state, data=FluTdC1971, verbose=TRUE)
	testFitmodel(fitmodel=SEITL_stoch, theta=theta, init.state=init.state, data=FluTdC1971, verbose=TRUE)

	init.state <- c("S"=280,"E"=0,"I"=2,"T1"=0,"T2"=0,"L"=4,"Inc"=0)

	testFitmodel(fitmodel=SEIT2L_det, theta=theta, init.state=init.state, data=FluTdC1971, verbose=TRUE)
	testFitmodel(fitmodel=SEIT2L_stoch, theta=theta, init.state=init.state, data=FluTdC1971, verbose=TRUE)

})
