# Implementations for cumputing fundamental variables. These functions will be internal and should NOT be
# exported.

#' @importFrom stats lm.fit lag na.omit
#' @importFrom RobStatTM lmrobdetMM lmrobdet.control

# Calculate pre-ranking beta, as outlined in Fama and French (1992)
preRankBeta <- function(x, preceding, periodicity, min.prec, type = "LS", ...) {
  stopifnot(type %in% c('classic', 'robust'))

  #
  if (nrow(x) < prec.min) {
    return(FALSE)
  }

  if (type == 'classic') {
    chkDots(...)
    
    if (nrow(x) < 24) {
      return(FALSE)
    }
    
    betas <- rollapply(data      = x,
                       width     = 60,
                       FUN       = "computeClassicDimsonBeta",
                       type      = type,
                       by.column = FALSE,
                       partial   = 24,
                       align     = "right")
  } else {
    betas <- rollapply(data      = x,
                       width     = 60,
                       FUN       = "computeClassicDimsonBeta",
                       type      = type,
                       by.column = FALSE,
                       partial   = 24,
                       align     = "right")
  }

  return(betas)
}

# Calculates post-ranking beta, as described in Fama and French (1992)
postRankBeta <- function(x, fromtype = "classic", ...) {
  stopifnot(type %in% c('classic', 'robust'))

  if (nrow(x) < 24) {
    return(FALSE)
  }
    
  start_date <- max(ceiling_date(from %m-% months(6), "year") %m+% months(6), as.Date("1963-07-31"))
    
  indx <- as.integer(seq(sum(x$date <= start_date), nrow(x), by = 1))

  if (type == 'classic') {
    chkDots(...)

    lapply(indx, function (i, data) { computeClassicDimsonBeta(data[1:i]) }, data = x)
  } else {
    lapply(indx, function (i, data) { computeRobustDimsonBeta(data[1:i]) }, data = x)
  }

  return(betas)
}

# Function to compute Dimson betas using least squares regression
computeClassicDimsonBeta <- function(x) {
  fit = lm.fit(x[, c("ind_ret", "lag_ind_ret")], x[, "adj_ret"])
  sum(fit$coefficients[2:3])
}

computeRobustDimsonBeta <- function(x, ...) {
  fit = lmrobdetMM(adj_ret ~ ind_ret + lag_ind_ret, data = x, ...)
  sum(fit$coefficients[2:3])
}
