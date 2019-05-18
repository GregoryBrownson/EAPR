# Implementations for cumputing fundamental variables. These functions will be internal and should NOT be
# exported.

#' @importFrom stats lm.fit
#' @importFrom RobStatTM lmrobdetMM lmrobdet.control

# Calculate pre-ranking beta, as outlined in Fama and French (1992)
preRankBeta <- function(x, preceding, periodicity, min.prec, type = "LS", ...) {
  stopifnot(type %in% c('classic', 'robust'))

  #
  if (nrow(x) < prec.min) {
    return(FALSE)
  }

  betas <- rollapply(data      = x,
                     preceding = 60,
                     FUN       = "compute_beta",
                     type      = type,
                     by.column = FALSE,
                     by        = 12,
                     partial   = 24,
                     align     = "right")

  return(betas)
}

# Calculates post-ranking beta, as described in Fama and French (1992)
postRankBeta <- function(x, fromtype = "classic", ...) {
  stopifnot(type %in% c('classic', 'robust'))

  if (nrow(x) < 24) {
    return(FALSE)
  }

  start <- ceiling_date(x[1, date] %m-% months(6), "year") %m+% months(6) - days(1)

  dates <-

  if (type == 'classic') {
    chkDots(...)

    betas <- rollapply(data      = x,
                       FUN       = computeClassicBeta,
                       by.column = FALSE,
                       by        = 12,
                       partial   = 24,
                       align     = "right")
  } else {
    lapply()
  }

  return(betas)
}

# Function to compute Dimson betas using least squares regression
computeClassicBeta <- function(x) {
  fit = lm.fit(x[, c("ind_ret", "lag_ind_ret")], x[, "adj_ret"])
  sum(fit$coefficients[2:3])
}

computeRobustBeta <- function(x, ...) {
  fit = lmrobdetMM(adj_ret ~ ind_ret + lag_ind_ret, data = x, ...)
  sum(fit$coefficients[2:3])
}
