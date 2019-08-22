#' Implementation of filter outlined in Fama and French (1992).
#' 
#' @importFrom data.table ':=' as.data.table
#' @importFrom lubridate year
#'
#' @title applyFilter
#'
#' @param x An eapr object
#' @param type Filter or list of filters to apply to data
#'
#' @export
#'

applyFilter <- function(x, type) {
  
  valid.filters <- c("ff")
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

filter.ff92 <- function(x) {
  # valid.subset <- x$ccm %>%
  #   group_by(permno, rebalance_date) %>%
  #   mutate(month = month(date),
  #          has_Jun = any(month == 6),
  #          has_Dec = any(month == 12)) %>%
  #   dplyr::filter(has_Jun == TRUE && has_Dec == TRUE && is_financial == 0) %>%
  #   ungroup %>%
  #   as.data.table
  
  dt <- x$ccm[, .(date, rebalance_date, permno, is_financial)]
  dt[, c("has_Jun", "has_Dec") := list(any(month(date) == 6), any(month(date) == 12)), by = list(rebalance_date, permno)]
  dt <- dt[has_Jun == TRUE & has_Dec == TRUE & is_financial == 0]
  dt[, rebalance_date := rebalance_date + years(1)]
  
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

  return(na.omit(x, cols = c("book_equity", "asset_market", "earnings_price")))
}

