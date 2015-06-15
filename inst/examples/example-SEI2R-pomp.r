## data(FluTdC1971)

SEI2R.sim.c <- '
    double rate[5];
    double dN[5];

    double nu = 1 / D_lat;
    double tau = 1 / D_not;
    double gamma = 1 / D_inf;

    double N = S + E1 + I2 + Ic + Ih + R;

    double beta_dt = rnorm(0, sqrt(dt));
    beta += beta_dt;

    rate[0] = beta * (Ic + Ih) / N;
    rate[1] = 2 * nu;
    rate[2] = 2 * nu;
    rate[3] = tau * rgamma(100, 0.01);
    rate[4] = gamma;

    reulermultinom(1, S, &rate[0], dt, &dN[0]);
    reulermultinom(1, E1, &rate[1], dt, &dN[1]);
    reulermultinom(1, E2, &rate[2], dt, &dN[2]);
    reulermultinom(1, Ic, &rate[3], dt, &dN[3]);
    reulermultinom(1, Ih, &rate[4], dt, &dN[4]);
    
    S += -dN[0];
    E1 += dN[0] - dN[1];
    E2 += dN[1] - dN[2];
    Ic += dN[2] - dN[3];
    Ih += dN[3] - dN[4];
    R += dN[4];
    Inc += dN[3];
'

SEI2R.skel.c <- '
    double trans[5];

    double beta = R0 / D_inf;
    double epsilon = 1 / D_lat;
    double nu = 1 / D_inf;
    double tau = 1 / D_imm;

    double N = S + E + I + T + L;

    trans[0] = beta * I / N * S;
    trans[1] = epsilon * E;
    trans[2] = nu * I;
    trans[3] = alpha * tau * T;
    trans[4] = (1 - alpha) * tau * T;

    DS = -trans[0] + trans[4];
    DE = trans[0] - trans[1];
    DI = trans[1] - trans[2];
    DT = trans[2] - trans[3] - trans[4];
    DL = trans[3];
    DInc = trans[1];
'

SEI2R.rmeas.c <- '
    obs = rnegbin_mu(rho * Inc, sqrt(rho * Inc + phi ^ 2 * rho ^ 2 * Inc ^ 2));
'

SEI2R.dmeas.c <- '
    lik = dnegbin_mu(obs, rho * Inc, sqrt(rho * Inc + phi ^ 2 * rho ^ 2 * Inc ^ 2, give_log);
'

SEI2R.dprior.c <- '
  lik = dunif(R0, 1, 50, 1) +
          dunif(D_lat, 0, 10, 1) +
          dunif(D_inf, 0, 15, 1) +
          dunif(D_imm, 0, 50, 1) +
          dunif(alpha, 0, 1, 1) +
          dunif(rho, 0, 1, 1);
  lik = (give_log) ? lik : exp(lik);    
'

SEI2R_pomp <- pomp(data = FluTdC1971[, c("time", "obs")],
                   rprocess = euler.sim(step.fun = Csnippet(SEI2R.sim.c), delta.t = 0.1),
                   dprior = Csnippet(SEI2R.dprior.c), 
                   rmeasure = Csnippet(SEI2R.rmeas.c),
                   dmeasure = Csnippet(SEI2R.dmeas.c),
                   skeleton = Csnippet(SEI2R.skel.c),
                   skeleton.type = "vectorfield", 
                   times = "time",
                   t0 = 1,
                   zeronames = "Inc", 
                   paramnames = c("R0", "D_inf", "D_lat", "D_imm", "alpha", "rho"),
                   statenames = c("S", "E", "I", "T", "L", "Inc"),
                   obsnames = c("obs"))
                   
