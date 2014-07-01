# Assumptions: 
# common R0
# 80% chance to develop long-term protection following recovery
# 80% of symptomatic and 85% reporting rate (rho = 0.8*0.85 ~ 0.7)
# Only the two islanders with symptoms at disembarkation were infectious
# No islander immune at the beginning of the epidemic
theta.guess1 <- c("R0"=2, "D.lat"=2 , "D.inf"=3, "alpha"=0.8, "D.imm"=15, "rho"=0.7)
state.init.guess1 <- c("S"=282,"E"=0,"I"=2,"T"=0,"L"=0,"Inc"=0)

# Assumptions: 
# high R0 (close knit community) => create a larger 1rst wave
# 80% chance to develop long-term protection following recovery
# 80% of symptomatic and 85% reporting rate (rho = 0.8*0.85 ~ 0.7)
# Only the two islanders with symptoms at disembarkation were infectious
# No islander immune at the beginning of the epidemic
theta.guess2 <- c("R0"=10, "D.lat"=2 , "D.inf"=3, "alpha"=0.8, "D.imm"=15, "rho"=0.70)
state.init.guess2 <- c("S"=282,"E"=0,"I"=2,"T"=0,"L"=0,"Inc"=0)

# Assumptions: 
# high R0 (close knit community)
# 50% chance to develop long-term protection following recovery (=> create a larger 2nd wave)
# 80% of symptomatic and 85% reporting rate (rho = 0.8*0.85 ~ 0.7)
# Only the two islanders with symptoms at disembarkation were infectious
# No islander immune at the beginning of the epidemic
theta.guess3 <- c("R0"=10, "D.lat"=2 , "D.inf"=3, "alpha"=0.5, "D.imm"=15, "rho"=0.70)
state.init.guess3 <- c("S"=282,"E"=0,"I"=2,"T"=0,"L"=0,"Inc"=0)
