#' Quantile portfolio formation
#'
#' This function creates a univariate or bivariate quantile portfolio
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

quantilePortfolio <- function(x, q, on, sort = "univariate") {
  # x should be eapr object
  stopifnot(class(x) == "eapr")

  # Check if value for sort is valid
  valid.sorts <- c("univariate", "bivariate.ind", "bivariate.dep")
  stopifnot(sort %in% c(valid.sorts))

  # Check if there are the correct number of variables
  if (sort == "univariate") {
    stopifnot(length(on) == 1)
  } else {
    stopifnot(length(on) == 2)
  }

  # Check if variables are in data table
  stopifnot(all(on %in% x$ccm))

  # Check if the value for q is valid
  if (is.vector(q)) {
    stopifnot(all(q >= 0.0 & q <= 1.0))
    q <- q[order(q)]
    q <- unique(c(0.0, q, 1.0))
  } else if (is.numeric(q)) {
    stopifnot(q >= 1)
    q <- seq(from = 0.0, to = 1.0, length.out = q + 2)
  } else {
    stop("Invalid option for q. Should be a vector of percentile cuts or an integer indicating the number of quantiles desired.")
  }

  if (sort == "univariate") {
    cuts <- quantile(x$ccm[, ..on], q)
  }
  
  portfolio <- list()

  return(portfolio)
}

quartilePortfolio <- function(x, on, sort = "uni") {
  return(quantilePortfolio(x, 4, on, sort))
}

quintilePortfolio <- function(x, on, sort = "uni") {
  return(quantilePortfolio(x, 5, on, sort))
}

decilePortfolio <- function(x, on, sort = "uni") {
  return(quantilePortfolio(x, 10, on, sort))
}
