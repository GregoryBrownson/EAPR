#' Implementation of filter outlined in Fama and French (1992).
#' 
#' @importFrom data.table ':=' as.data.table
#' @importFrom lubridate year
#' @importFrom dplyr group_by arrange mutate ungroup
#'
#' @title filter
#'
#' @param x eapr object
#' @param type Single filter or list of filters
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
  x$ccm <- x$ccm %>%
    group_by(permno, rebalance_date) %>%
    mutate(month = month(date),
           has_Jun = any(month == 6),
           has_Dec = any(month == 12)) %>%
    dplyr::filter(has_Jun == TRUE && has_Dec == TRUE && is_financial == 0) %>%
    ungroup %>%
    as.data.table

  x$ccm[, c("month", "has_Jun", "has_Dec", "is_financial") := NULL]
  
  valid.subset <- x$market.dt %>%
    group_by(permno) %>%
    arrange(date) %>%
    mutate(count = seq(n())) %>%
    ungroup %>%
    group_by(permno, rebalance_date) %>%
    mutate(valid_obs = any(count >= 24)) %>%
    dplyr::filter(valid_obs == TRUE) %>%
    ungroup %>%
    as.data.table
  
  valid.subset[, rebalance_date := rebalance_date + years(1)]
  
  valid.subset <- unique(valid.subset[, .(permno, rebalance_date)])
  
  x$ccm <- merge(x$ccm, valid.subset[, .(permno, rebalance_date)], by = c("permno", "rebalance_date"))
  
  x$market.dt <- x$market.dt[permno %in% unique(x$ccm$permno)]

  return(na.omit(x, cols = c("book_equity", "asset_market")))
}

