data(FluTdC1971)

SEITL.sim.c <- '
    double rate[5];
    double dN[5];

    double beta = R0 / D_inf;
    double epsilon = 1 / D_lat;
    double nu = 1 / D_inf;
    double tau = 1 / D_imm;

    rate[0] = beta * I / N;
    rate[1] = epsilon;
    rate[2] = nu;
    rate[3] = alpha * tau;
    rate[4] = (1 - alpha) * tau;

    reulermultinom(1, S, &rate[0], dt, &dN[0]);
    reulermultinom(1, E, &rate[1], dt, &dN[1]);
    reulermultinom(1, I, &rate[2], dt, &dN[2]);
    reulermultinom(2, T, &rate[3], dt, &dN[3]);
    
    S += -dN[0] + dN[4];
    E += dN[0] - dN[1];
    I += dN[1] - dN[2];
    T += dN[2] - dN[3] - dN[4];
    L += dN[3];
    Inc += dN[1];
'

SEITL.rmeas.c <- '
    obs = rpois(rho * Inc);
'

SEITL.dmeas.c <- '
    lik = dpois(obs, rho * Inc, give_log);
'

SEITL_pomp <- pomp(data = FluTdC1971,
                   rprocess = euler.sim(step.fun = Csnippet(SEITL.sim.c), delta.t = 0.1),
                   rmeasure = Csnippet(SEITL.rmeas.c),
                   dmeasure = Csnippet(SEITL.dmeas.c),
                   times = "time",
                   t0 = 1,
                   paramnames = c("R0", "D_inf", "D_lat", "D_imm", "N", "alpha", "rho"),
                   statenames = c("S", "E", "I", "T", "L", "Inc"),
                   obsnames = c("obs"))
                   
