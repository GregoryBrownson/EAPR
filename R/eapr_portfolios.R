eapr.quantilePortfolio <- function(x, q, on, sort = "uni") {
  # Check if value for sort is valid
  valid.sorts <- c("uni", "bi.ind", "bi.dep")
  stopifnot(type %in% c(valid.sorts))

  # Check if there are the correct number of variables
  if (type == "uni") {
    stopifnot(length(on) == 1)
  } else {
    stopifnot(length(on) == 2)
  }

  # Check if variables are in data table
  stopifnot(all(on %in% x$dt))

  # Check if the value for q is valid
  if (is.vector(q)) {
    stopifnot(all(q >= 0.0 & q <= 1.0))
    q <- q[order(q)]
    q <- unique(c(0.0, q, 1.0))
  } else if (is.numeric(q)) {
    stopifnot(q > 1)
    q <- seq(from = 0.0, to = 1.0, length.out = q + 1)
  } else {
    stop("Invalid option for q. Should be a vector of percentile cuts or an integer indicating the number of quantiles desired.")
  }

  portfolio <- list()

  return(portfolio)
}

eapr.quartilePortfolio <- function(x, on, sort = "uni") {
  return(quantilePortfolio(x, 4, on, sort))
}

eapr.quintilePortfolio <- function(x, on, sort = "uni") {
  return(quantilePortfolio(x, 5, on, sort))
}

eapr.decilePortfolio <- function(x, on, sort = "uni") {
  return(quantilePortfolio(x, 10, on, sort))
}