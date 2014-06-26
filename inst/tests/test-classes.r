context("classes")

test_that("constructor for SEITL model",{

# load and rename data
	
	SEITL_det <- SEITL_createModelTdC(simulate="deterministic")
	expect_true(inherits(SEITL_det,"fitmodel"))

	SEITL_sto <- SEITL_createModelTdC(simulate="stochastic")
	expect_true(inherits(SEITL_sto,"fitmodel"))

	SEIT2L_det <- SEIT2L_createModelTdC(simulate="deterministic")
	expect_true(inherits(SEIT2L_det,"fitmodel"))

	SEIT2L_sto <- SEIT2L_createModelTdC(simulate="stochastic")
	expect_true(inherits(SEIT2L_sto,"fitmodel"))


	## test them
	theta <- c("R0"=10, "LP"=2 , "IP"=3, "alpha"=0.5, "TIP"=15, "rho"=0.7)
	state.init <- c("S"=280,"E"=0,"I"=2,"T"=0,"L"=4,"Inc"=0)

	data("FluTdC1971",envir = environment())
	data <- rename(FluTdC1971,c("day"="time","incidence"="Inc"))[c("time","Inc")]

	testFitmodel(model=SEITL_det, theta=theta, state.init=state.init, data=data, verbose=TRUE)
	testFitmodel(model=SEITL_sto, theta=theta, state.init=state.init, data=data, verbose=TRUE)

	state.init <- c("S"=280,"E"=0,"I"=2,"T1"=0,"T2"=0,"L"=4,"Inc"=0)

	testFitmodel(model=SEIT2L_det, theta=theta, state.init=state.init, data=data, verbose=TRUE)
	testFitmodel(model=SEIT2L_sto, theta=theta, state.init=state.init, data=data, verbose=TRUE)

})