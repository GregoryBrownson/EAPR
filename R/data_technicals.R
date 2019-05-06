# Implementations for cumputing fundamental variables. These functions will be internal and should NOT be
# exported.

# Calculate pre-ranking beta, as outlined in Fama and French (1992)
preRankBeta(dat.grouped, ret.dt, market.dt, preceding, periodicity) <- function(x, type = "LS") {
  betas <- rollapply(data      = x,
                     preceding = 60,
                     FUN       = "compute.beta",
                     type      = type,
                     by.column = FALSE,
                     by        = 12,
                     partial   = 24,
                     align     = "right")

  return(betas)
}

# Calculates post-ranking beta, as described in Fama and French (1992)
postRankBeta(x) <- function(x, type = "LS") {
  betas <- rollapply(data      = x,
                     FUN       = "compute.beta",
                     type      = type,
                     by.column = FALSE,
                     by        = 12,
                     partial   = 24,
                     align     = "right")
  
  return(betas)
}

# Function to compute Dimson betas using least squares regression
computeClassicBeta <- function(x) {
  fit = lm.fit(z[c("ind_ret", "lag_ind_ret")], z["rets"])
  sum(fit$coefficients[2:3])
}

computeRobustBeta <- function(x) {
  #TODO: Implement computation of robust betas
}