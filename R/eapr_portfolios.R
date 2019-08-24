#' Quantile portfolio formation
#'
#'
#' @description This function creates a univariate or bivariate quantile portfolio
#'
#' @param x An eapr object which contains the time series of variables for all
#' stocks.
#' @param q Either a vector of percentile cut points or the number of quantiles
#' to compute. If the latter, quantiles are computed using evenly spaced percentile
#' cut points.
#' @param on Variable or variables to sort on (max of 2).
#' @param sort Type of sort for portfolios. Options are univariate, bivariate.ind
#' (independent bivariate), and bivariate.dep (dependent bivariate) sorts.
#'
#' @export

# TODO: Customize the variables to allow names such as 'Size' and 'Beta' instead of actual variable names

quantilePortfolio <- function(x, q, on, sort = "univariate") {
  # x should be eapr object
  stopifnot(class(x) == "eapr")

  # Check if value for sort is valid
  valid.sorts <- c("univariate", "bivariate.ind", "bivariate.dep")
  stopifnot(sort %in% valid.sorts)

  # Check if there are the correct number of variables
  if (sort == "univariate") {
    stopifnot(length(on) == 1)
  } else {
    stopifnot(length(on) == 2)
  }

  # Check if variables are in data table
  stopifnot(all(on %in% x$ccm))

  # Check if the value for q is valid
  if (length(q) == 1) {
    stopifnot(q >= 1)
    q <- seq(from = 0.0, to = 1.0, length.out = q + 2)
  } else if (is.numeric(q)) {
    stopifnot(all(q >= 0.0 & q <= 1.0))
    q <- q[order(q)]
    q <- unique(c(0.0, q, 1.0))
  } else {
    stop("Invalid option for q. Should be a vector of percentile cuts or an integer indicating the number of quantiles desired.")
  }
  
  cols <- c("rebalance_date", "permno", "exchange_code", on)
  
  dat.split <- split(x$ccm[month(date) == 7, ..cols], x$ccm[month(date) == 7]$rebalance_date)
  
  n <- length(q) - 1

  if (sort == "univariate") {
    
    dat.list <- lapply(dat.split, function(dt, col, q) {
      dt[[paste0(col, "_quantile")]] <- cut(dt[[col]], breaks = quantile(na.omit(dt[exchange_code == 1][[col]]), probs = q), labels = 1:n, right = FALSE)
      cols <- c("rebalance_date", "permno", paste0(on, "_quantile"))
      dt[, ..cols]
    },
    col = on,
    q = q)
    dat <- Reduce(rbind, dat.list)
  } else if (sort == "bivariate.ind") {
    dat.list <- lapply(dat.split, function(dt, col, q) {
      dt[[paste0(col[1], "_quantile")]] <- cut(dt[[col[1]]], breaks = quantile(na.omit(dt[exchange_code == 1][[col[1]]]), probs = q), labels = 1:n, right = FALSE)
      dt[[paste0(col[2], "_quantile")]] <- cut(dt[[col[2]]], breaks = quantile(na.omit(dt[exchange_code == 1][[col[2]]]), probs = q), labels = 1:n, right = FALSE)
      cols <- c("rebalance_date", "permno", paste0(on[1], "_quantile"), paste0(on[2], "_quantile"))
      dt[, ..cols]
    },
    col = on,
    q = q)
    dat <- Reduce(rbind, dat.list)
  } else {
    dat.list <- lapply(dat.split, function(dt, col, q) {
      dt[[paste0(col[1], "_quantile")]] <- cut(dt[[col[1]]], breaks = quantile(na.omit(dt[exchange_code == 1][[col[1]]]), probs = q), labels = 1:n, right = FALSE)
      dt.list <- lapply(split(dt, dt[[paste0(on[1], "_quantile")]]), function(dt, col, q) {
        dt[[paste0(col, "_quantile")]] <- cut(dt[[col]], breaks = quantile(na.omit(dt[exchange_code == 1][[col]]), probs = q), labels = 1:n, right = FALSE)
        dt
      },
      col = col[2],
      q = q)
      
      dt <- Reduce(rbind, dt.list)
      cols <- c("rebalance_date", "permno", paste0(on[1], "_quantile"), paste0(on[2], "_quantile"))
      dt[, ..cols]
    },
    col = on,
    q = q)
    dat <- Reduce(rbind, dat.list)
  }
  
  z <- list(portfolios = dat, var = on, type = sort)
  class(z) <- "eaprPortfolio"

  return(z)
}

quartilePortfolio <- function(x, on, sort = "univariate") {
  return(quantilePortfolio(x, 4, on, sort))
}

quintilePortfolio <- function(x, on, sort = "univariate") {
  return(quantilePortfolio(x, 5, on, sort))
}

decilePortfolio <- function(x, on, sort = "univariate") {
  return(quantilePortfolio(x, 10, on, sort))
}

# Calculates post-ranking beta for returns on sorted portfolios, as described in Fama and French (1992)

portfolio_means.eapr <- function(x, p, response, excess = "vw_index") {
  stopifnot(class(p) == "eaprPortfolio")
  
  cols <- c("rebalance_date", "permno", p$on, response)
  dat <- dat[, ..cols]
  
  if ("ret" %in% response) {
    if (excess == "vw_index") {
      merge(dat, unique(x$market.dt[, .(rebalance_date, vwind_ret)]), by = "rebalance_date")
      dat[, excess_ret := get(response) - vwind_ret]
    } else if (excess == "ew") {
      dat[, eqind_ret := sum(get(response)), by = list(date)]
      dat[, excess_ret := get(response) - eqind_ret]
    }
  }
  
}

portfolio_stdev.eapr <- function(x, p, response) {
  
}

postRankBeta <- function(x, p, preceding, min.prec, type = "classic") {
  stopifnot(all(type %in% c('classic', 'robust')))
  
  min <- as.integer(preceding * min.prec)
  
  if (nrow(x) < min) {
    return(data.table(date = x$date, post_rank_beta = NA))
  }
  
  start_date <- max(ceiling_date(x$date[1] %m-% months(6), "year") %m+% months(6) - days(1), as.Date("1963-06-30"))
  
  indx <- seq(max(min, as.integer(sum(x$date <= start_date))), nrow(x), by = 1)
  
  betas <- lapply(indx, function (i, data, type) { DimsonBeta(data[1:i], type) },
                  data = x,
                  type = type)
  
  names <- paste0("post_rank_beta_", names(betas[[1]]))
  
  betas <- matrix(unlist(betas), ncol = length(type), byrow = TRUE)
  
  colnames(betas) <- names
  
  betas <- data.table(x[indx, "date"], betas)
  
  return(betas)
}