# Implementations for cumputing fundamental variables. These functions will be internal and should NOT be
# exported.

#' @importFrom lubridate ceiling_date '%m-%' '%m+%' days
#' @importFrom stats lm.fit lag na.omit
#' @importFrom RobStatTM lmrobdetMM lmrobdet.control
#' @importFrom zoo rollapply

# Calculate pre-ranking beta, as outlined in Fama and French (1992)
preRankBeta <- function(x, preceding, min.prec, type = "classic", ...) {
  stopifnot(all(type %in% c('classic', 'robust')))

  min <- as.integer(preceding * min.prec)

  if (nrow(x) < min) {
    return(data.table(date = x$date, pre_rank_beta = NA))
  }

  start_date <- max(ceiling_date(x$date[1] %m-% months(6), "year") %m+% months(6) - days(1), as.Date("1963-06-30"))

  indx <- seq(max(min, as.integer(sum(x$date <= start_date))), nrow(x), by = 1)


  betas <- lapply(indx, function(i, dt, width, type, ...) {
             start <- max(1, i - width + 1)
             computeDimsonBeta(dt[start:i], type, ...)
           },
           dt    = x,
           width = preceding,
           type  = type)

  names <- paste0("pre_rank_beta_", names(betas[[1]]))

  betas <- matrix(unlist(betas), ncol = length(type), byrow = TRUE)

  colnames(betas) <- names

  betas <- data.table(x[indx, "date"], betas)

  return(betas)
}

# Calculates post-ranking beta, as described in Fama and French (1992)
postRankBeta <- function(x, preceding, min.prec, type = "classic", ...) {
  stopifnot(all(type %in% c('classic', 'robust')))

  min <- as.integer(preceding * min.prec)

  if (nrow(x) < min) {
    return(data.table(date = x$date, post_rank_beta = NA))
  }

  start_date <- max(ceiling_date(x$date[1] %m-% months(6), "year") %m+% months(6) - days(1), as.Date("1963-06-30"))

  indx <- seq(max(min, as.integer(sum(x$date <= start_date))), nrow(x), by = 1)

  betas <- lapply(indx, function (i, data, type, ...) { computeDimsonBeta(data[1:i], type, ...) },
                  data = x,
                  type = type,
                  ...)

  names <- paste0("post_rank_beta_", names(betas[[1]]))

  betas <- matrix(unlist(betas), ncol = length(type), byrow = TRUE)

  colnames(betas) <- names

  betas <- data.table(x[indx, "date"], betas)

  return(betas)
}

# Function to compute Dimson betas using least squares regression
# TODO: Generalize this function
computeDimsonBeta <- function(x, type, ...) {
  beta <- list()

  if ("classic" %in% type) {
    fit = lm.fit(cbind(1, as.matrix(x[, c("ind_ret", "lag_ind_ret")])), as.matrix(x[, "adj_ret"]))
    beta$classic <- sum(fit$coefficients[2:3])
  }

  if ("robust" %in% type) {
    fit = lmrobdetMM(adj_ret ~ ind_ret + lag_ind_ret, data = x, ...)
    beta$robust <- sum(fit$coefficients[2:3])
  }

  return(beta)
}

# Computes CAPM beta
computeCAPMBeta <- function(x) {
  beta <- list()

  if ("classic" %in% type) {
    fit = lm.fit(cbind(1, as.matrix(x[, "ind_ret"])), as.matrix(x[, "adj_ret"]))
    beta$classic <- fit$coefficients[2]
  }

  if ("robust" %in% type) {
    fit = lmrobdetMM(adj_ret ~ ind_ret, data = x, ...)
    beta$robust <- fit$coefficients2
  }

  return(beta)
}
