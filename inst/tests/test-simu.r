context("simu")

test_that("simulate and generate observation",{

	# SEITL
	SEITL_deter <- SEITL_createFitmodel("deterministic")
	SEITL_sto <- SEITL_createFitmodel("stochastic")
	list_model <- list(SEITL_deter,SEITL_sto)

	theta <- c("R0"=10, "LP"=2 , "IP"=3, "alpha"=0.5, "TIP"=15, "rho"=0.7)
	state.init <- c("S"=280,"E"=0,"I"=2,"T"=0,"L"=4,"Inc"=0)
	times <- 0:58

	for(SEITL in list_model){
		
		traj <- SEITL$simulate(theta=theta, state.init, times=times)
		expect_true(inherits(traj,"data.frame"))

		traj.obs <- genObsTraj(SEITL, theta, state.init, times)
		expect_true(inherits(traj.obs,"data.frame"))

	}
	
	# SEITL2
	SEIT2L_deter <- SEIT2L_createFitmodel("deterministic")
	SEIT2L_sto <- SEIT2L_createFitmodel("stochastic")
	list_model <- list(SEIT2L_deter,SEIT2L_sto)

	state.init <- c("S"=280,"E"=0,"I"=2,"T1"=0,"T2"=0,"L"=4,"Inc"=0)

	for(SEIT2L in list_model){
		
		traj <- SEIT2L$simulate(theta=theta, state.init, times=times)
		expect_true(inherits(traj,"data.frame"))

		traj.obs <- genObsTraj(SEIT2L, theta, state.init, times)
		expect_true(inherits(traj.obs,"data.frame"))

	}

})
