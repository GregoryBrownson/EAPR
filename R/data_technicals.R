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

compute.beta <- function(x, type) {
  fit = lm.fit(z[c("ind_ret", "lag_ind_ret")], z["rets"])
  sum(fit$coefficients[2:3])
}