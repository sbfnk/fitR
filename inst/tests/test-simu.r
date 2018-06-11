context("simu")

test_that("simulate and generate observation",{

	# SEITL
	data(SEITL_deter)
	data(SEITL_stoch)
	list_model <- list(SEITL_deter,SEITL_stoch)

	theta <- c("R0"=10, "D_lat"=2 , "D_inf"=3, "alpha"=0.5, "D_imm"=15, "rho"=0.7)
	init.state <- c("S"=280,"E"=0,"I"=2,"T"=0,"L"=4,"Inc"=0)
	times <- 0:58

	for(SEITL in list_model){
		
		traj <- SEITL$simulate(theta=theta, init.state, times=times)
		expect_true(inherits(traj,"data.frame"))

		traj.obs <- rTrajObs(SEITL, theta, init.state, times)
		expect_true(inherits(traj.obs,"data.frame"))

	}
	
	# SEITL2
	data(SEIT2L_deter)
	data(SEIT2L_stoch)
	list_model <- list(SEIT2L_deter,SEIT2L_stoch)

	init.state <- c("S"=280,"E"=0,"I"=2,"T1"=0,"T2"=0,"L"=4,"Inc"=0)

	for(SEIT2L in list_model){
		
		traj <- SEIT2L$simulate(theta=theta, init.state, times=times)
		expect_true(inherits(traj,"data.frame"))

		traj.obs <- rTrajObs(SEIT2L, theta, init.state, times)
		expect_true(inherits(traj.obs,"data.frame"))

	}

})
