# Implementation of some generic filters used for asset pricing in the past.
#' Filtering data
#' 
#' @importFrom data.table ':=' as.data.table
#' @importFrom lubridate year
#'
#' @export applyFilter
#' @rdname applyFilter
#'
#' @param x An eapr object
#' @param type Filter or list of filters to apply to dataset. Currently, only the filter used in Fama French 1992 is implemented.
#'
#' @return A filtered EAPR object
#' 

applyFilter <- function(x, type) {
  
  valid.filters <- c("ff92")
  if (!all(type %in% valid.filters)) {
    invalid.filters <- type[!(type %in% valid.filters)]
    if (length(invalid.filters) == 1) {
      stop(paste0(invalid.filters, " is not a valid filter!"))
    } else {
      stop(paste0(invalid.filters[1:(length(invalid.filters)-1)], " and ", invalid.filters[length(invalid.filters)], " are not valid filters!", collapse = ", "))
    }
  }
  
  for (i in 1:length(type)) {
    x <- do.call(paste("filter", type[i], sep = '.'), list(x))
  }

  return(x)
}

#' Filtering data using methods detailed in Fama French (1992)
#' 
#' @importFrom data.table ':=' as.data.table
#' @importFrom lubridate year
#'
#' @export ff92Filter
#' @rdname ff92Filter
#'
#' @param x An eapr object
#'
#' @return A filtered EAPR object using the same filtering methods used in Fama French (1992)
#' 

ff92Filter <- function(x) {
  # valid.subset <- x$ccm %>%
  #   group_by(permno, rebalance_date) %>%
  #   mutate(month = month(date),
  #          has_Jun = any(month == 6),
  #          has_Dec = any(month == 12)) %>%
  #   dplyr::filter(has_Jun == TRUE && has_Dec == TRUE && is_financial == 0) %>%
  #   ungroup %>%
  #   as.data.table
  
  dt <- x$ccm[, .(date, rebalance_date, permno, log_size, is_financial)]
  dt[, has_Jun := !is.null(log_size), by = list(rebalance_date, permno)]
  dt <- dt[has_Jun == TRUE & is_financial == 0]
  dt[, rebalance_date := rebalance_date]
  
  dt <- unique(dt[, .(permno, rebalance_date)])
  
  x$ccm <- merge(x$ccm, dt[, .(permno, rebalance_date)], by = c("permno", "rebalance_date"))
  
  x$ccm[, c("is_financial") := NULL]
  
  # valid.subset <- x$market.dt %>%
  #   group_by(permno) %>%
  #   arrange(date) %>%
  #   mutate(count = seq(n())) %>%
  #   ungroup %>%
  #   group_by(permno, rebalance_date) %>%
  #   mutate(valid_obs = any(count >= 24)) %>%
  #   dplyr::filter(valid_obs == TRUE) %>%
  #   ungroup %>%
  #   as.data.table
  
  dt <- x$market.dt[, .(date, rebalance_date, permno)]
  dt[, rebalance_date := rebalance_date + years(1)]
  dt[order(date), count := seq(from = 1, to = .N), by = list(permno)]
  dt[, valid_obs := any(count >= 24), by = list(permno, rebalance_date)]
  dt <- dt[valid_obs == TRUE]
  
  x$ccm <- merge(x$ccm, unique(dt[, .(permno, rebalance_date)]), by = c("permno", "rebalance_date"))
  
  x$market.dt <- x$market.dt[permno %in% unique(x$ccm$permno)]
  
  cols <- intersect(c("book_market", "asset_market", "earnings_price"), colnames(x$ccm))

  return(na.omit(x, cols = cols))
}