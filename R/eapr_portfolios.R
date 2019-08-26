#' Quantile portfolio formation
#'
#' @rdname quantilePortfolio
#' @export quantilePortfolio
#'
#' @description This function creates a univariate or bivariate quantile portfolio
#'
#' @param x An eapr object which contains the time series of variables for all
#' stocks.
#' @param q Either a vector of percentile cut points or the number of quantiles
#' to compute. If the latter, quantiles are computed using evenly spaced percentile
#' cut points.
#' @param on.x First variable to sort on.
#' @param on.y Second variable to sort on. This option is ignored if 'univariate' sorting is selected.
#' @param sort Type of sort for portfolios. Options are univariate, bivariate.ind
#' (independent bivariate), and bivariate.dep (dependent bivariate) sorts.
#' 
#' @return An eaprPortfolio object containing a data.table of the time series of portfolio assignments for each stock in each period.

# TODO: Customize the variables to allow names such as 'Size' and 'Beta' instead of actual variable names

quantilePortfolio <- function(x, q, on.x, on.y = NULL, sort = "univariate") {
    # x should be eapr object
    stopifnot(class(x) == "eapr")
    
    # Check if value for sort is valid
    valid.sorts <- c("univariate", "bivariate.ind", "bivariate.dep")
    stopifnot(sort %in% valid.sorts)
    
    # Check if there are the correct number of variables
    if (sort == "univariate") {
        if (is.null(on.y)) {
            on <- on.x
        } else {
            sort <- "bivariate.ind"
            on <- c(on.x, on.y)
        }
    } else {
        stopifnot(!is.null(on.y))
        on <- c(on.x, on.y)
    }
    
    # Check if variables are in data table
    stopifnot(all(on %in% colnames(x$ccm)))
    
    # Check if the value for q is valid
    if (length(q) == 1) {
        stopifnot(q >= 2)
        q <- seq(from = 0, to = 1, length.out = q + 1)
    } else if (is.numeric(q)) {
        stopifnot(all(q >= 0 & q <= 1))
        q <- q[order(q)]
        q <- unique(c(0, q, 1))
    } else {
        stop("Invalid option for q. Should be a vector of percentile cuts or an integer indicating the number of quantiles desired.")
    }
    
    cols <- c("rebalance_date", "permno", "exchange_code", on)
    
    start_month <- unique(month(x$ccm$rebalance_date)) + 1
    
    dat.split <- split(x$ccm[month(date) == start_month, ..cols], x$ccm[month(date) == start_month]$rebalance_date)
    
    n <- length(q) - 1
    
    if (sort == "univariate") {
        
        dat.list <- lapply(dat.split, function(dt, col, q) {
            dt[[paste0(col, "_quantile")]] <- cut(dt[[col]], breaks = quantile(na.omit(dt[exchange_code == 1][[col]]), probs = q), labels = 1:n, right = FALSE)
            cols <- c("rebalance_date", "permno", paste0(on, "_quantile"))
            dt[, ..cols]
        }, col = on, q = q)
        dat <- Reduce(rbind, dat.list)
    } else if (sort == "bivariate.ind") {
        dat.list <- lapply(dat.split, function(dt, col, q) {
            dt[[paste0(col[1], "_quantile")]] <- cut(dt[[col[1]]], breaks = quantile(na.omit(dt[exchange_code == 1][[col[1]]]), probs = q), labels = 1:n, right = FALSE)
            dt[[paste0(col[2], "_quantile")]] <- cut(dt[[col[2]]], breaks = quantile(na.omit(dt[exchange_code == 1][[col[2]]]), probs = q), labels = 1:n, right = FALSE)
            cols <- c("rebalance_date", "permno", paste0(on[1], "_quantile"), paste0(on[2], "_quantile"))
            dt[, ..cols]
        }, col = on, q = q)
        dat <- Reduce(rbind, dat.list)
    } else {
        dat.list <- lapply(dat.split, function(dt, col, q) {
            dt[[paste0(col[1], "_quantile")]] <- cut(dt[[col[1]]], breaks = quantile(na.omit(dt[exchange_code == 1][[col[1]]]), probs = q), labels = 1:n, right = FALSE)
            dt.list <- lapply(split(dt, dt[[paste0(on[1], "_quantile")]]), function(dt, col, q) {
                dt[[paste0(col, "_quantile")]] <- cut(dt[[col]], breaks = quantile(na.omit(dt[exchange_code == 1][[col]]), probs = q), labels = 1:n, right = FALSE)
                dt
            }, col = col[2], q = q)
            
            dt <- Reduce(rbind, dt.list)
            cols <- c("rebalance_date", "permno", paste0(on[1], "_quantile"), paste0(on[2], "_quantile"))
            dt[, ..cols]
        }, col = on, q = q)
        dat <- Reduce(rbind, dat.list)
    }
    
    z <- list(portfolios = dat, var = on, type = sort)
    class(z) <- "eaprPortfolio"
    
    return(z)
}

#' @rdname quantilePortfolio
#' @export
quartilePortfolio <- function(x, on.x, on.y = NULL, sort = "univariate") {
    return(quantilePortfolio(x, 4, on.x, on.y, sort))
}

#' @rdname quantilePortfolio
#' @export
quintilePortfolio <- function(x, on.x, on.y = NULL, sort = "univariate") {
    return(quantilePortfolio(x, 5, on.x, on.y, sort))
}

#' @rdname quantilePortfolio
#' @export
decilePortfolio <- function(x, on.x, on.y = NULL, sort = "univariate") {
    return(quantilePortfolio(x, 10, on.x, on.y, sort))
}

portfolioMean <- function(x, p, response, excess = "vw_index") {
    stopifnot(class(p) == "eaprPortfolio")
    
    cols <- c("rebalance_date", "permno", p$on, response)
    dat <- dat[, ..cols]
    
    if ("ret" %in% response) {
        if (excess == "vw_index") {
            merge(dat, unique(x$market.dt[, .(rebalance_date, vwind_ret)]), by = "rebalance_date")
            dat[, `:=`(excess_ret, get(response) - vwind_ret)]
        } else if (excess == "ew") {
            dat[, `:=`(eqind_ret, sum(get(response))), by = list(date)]
            dat[, `:=`(excess_ret, get(response) - eqind_ret)]
        }
    }
    
}

portfolioStDev <- function(x, p, response) {
    
}

postRankBeta <- function(x, p, preceding, min.prec, type = "classic") {
    stopifnot(all(type %in% c("classic", "robust")))
    
    min <- as.integer(preceding * min.prec)
    
    if (nrow(x) < min) {
        return(data.table(date = x$date, post_rank_beta = NA))
    }
    
    start_date <- max(ceiling_date(x$date[1] %m-% months(6), "year") %m+% months(6) - days(1), as.Date("1963-06-30"))
    
    indx <- seq(max(min, as.integer(sum(x$date <= start_date))), nrow(x), by = 1)
    
    betas <- lapply(indx, function(i, data, type) {
        DimsonBeta(data[1:i], type)
    }, data = x, type = type)
    
    names <- paste0("post_rank_beta_", names(betas[[1]]))
    
    betas <- matrix(unlist(betas), ncol = length(type), byrow = TRUE)
    
    colnames(betas) <- names
    
    betas <- data.table(x[indx, "date"], betas)
    
    return(betas)
}
