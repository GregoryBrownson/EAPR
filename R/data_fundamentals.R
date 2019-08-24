# Implementations of fundamental variable computations. These functions will be internal and should NOT be
# exported.

#' @importFrom data.table set ':=' as.data.table setnames
#' @importFrom dplyr summarize
#' @importFrom lubridate month

bookEquity <- function(dt) {
  # comp
  # Dependencies: pstkrv, pstkl, pstk, txditc, seq

  dt[["pref_stock"]] <- ifelse(is.na(dt$pstkrv), dt$pstkl, dt$pstkrv)
  dt[["pref_stock"]] <- ifelse(is.na(dt$pref_stock), dt$pstk, dt$pref_stock)
  set(dt, which(is.na(dt[["pref_stock"]])), "pref_stock", 0)

  set(dt, which(is.na(dt[["txditc"]])), "txditc", 0)
  
  dt[["seq"]] <- ifelse(is.na(dt$seq), dt$bkvlps + dt$upstk, dt$seq)
  dt[["seq"]] <- ifelse(is.na(dt$seq), dt$assets - dt$lt, dt$seq)

  dt[["book_equity"]] <- dt$seq + dt$txditc - dt$pref_stock
  set(dt, which(dt[["book_equity"]] <= 0), "book_equity", NaN)
  
  dt[["log_book_equity"]] <- log(dt$book_equity)

  return(dt)
}

marketEquity <- function(dt) {
  # crsp
  # Dependencies: prc, shrout
  dt[["market_equity"]] <- abs(dt$price) * dt$shares_out

  dt.grouped <- group_by(dt, date, permco)

  max.me <- as.data.table(summarize(dt.grouped,
                                    sum_me = sum(market_equity),
                                    market_equity = max(market_equity)))

  dt <- merge(dt, max.me, by = c("date", "permco", "market_equity"))

  dt[, ':='(market_equity = sum_me,
            sum_me        = NULL)]
  
  set(dt, which(dt[["market_equity"]] <= 0), "market_equity", NaN)
  dt[["log_market_equity"]] <- log(dt$market_equity / 1000)
  
  rebalance_dates <- sort(unique(dt$rebalance_date))[1:2]
  
  if (rebalance_dates[1] + years(1) == rebalance_dates[2]) {
    dt.dec <- dt[month(date) == 12, .(permno, rebalance_date, market_equity, log_market_equity)]
    setnames(dt.dec, c("market_equity", "log_market_equity"), c("dec_market_equity", "log_dec_market_equity"))
    
    dt.dec[, rebalance_date := rebalance_date + years(1)]
    
    dt <- merge(dt, dt.dec, by = c("rebalance_date", "permno"), all.x = TRUE)
  }
  
  size <- dt[date == rebalance_date, .(permno, rebalance_date, market_equity, log_market_equity)]
  size[, rebalance_date := rebalance_date + years(1)]
  setnames(size, c("market_equity", "log_market_equity"), c("size", "log_size"))
  dt <- merge(dt, size, by = c("rebalance_date", "permno"), all.x = TRUE)
  
  return(dt)
}

bookToMarket <- function(dt) {
  # merged
  dt[["book_market"]] <- dt$book_equity / dt$dec_market_equity * 1000

  dt[["log_book_market"]] <- log(dt$book_market)

  return(dt)
}

assetToBook <- function(dt) {
  # comp
  # Dependencies: at

  dt[["asset_book"]] <- dt$assets / dt$book_equity

  dt[["log_asset_book"]] <- log(dt$asset_book)

  return(dt)
}

assetToMarket <- function(dt) {
  # merged
  # Dependencies: at

  dt[["asset_market"]] <- dt$assets / dt$dec_market_equity * 1000

  dt[["log_asset_market"]] <- log(dt$asset_market)

  return(dt)
}

earningsToPrice <- function(dt) {
  # merged
  # Dependencies: ib, prc
  
  dt[["earnings_price"]] <- dt$earnings / abs(dt$dec_market_equity) * 1000
  
  dt[["earnings_price_indicator"]] <- as.integer(dt$earnings_price < 0)
  
  dt[["positive_earnings_price"]] <- ifelse(dt$earnings_price >= 0, dt$earnings_price, 0)

  return(dt)
}

# comp
operatingProfitability <- function(dt) {
  # comp
  # dependencies: ebitda, xint
  dt[["oper_prof"]] <- (dt$ebitda - dt$interest_exp) / dt$book_equity

  return(dt)
}

investment <- function(dt) {
  # comp
  # dependencies: act
  dt[["investment"]] <- (dt$assets - lag(dt$assets)) / dt$assets

  return(dt)
}

cashFlowToPrice <- function(dt) {
  # merged
  # dependencies: act, ib, txdc
  dt[["equity_share"]] <- dt$dec_market_equity / (dt$assets - dt$book_equity + dt$dec_market_equity)

  dt[["cf_price"]] <- (dt$earnings + dt$equity_share * dt$depreciation + dt$deferred_tax) / dt$dec_market_equity

  return(dt)
}

dividendYield <- function(dt) {
  # crsp
  dt.jun <- dt[month(date) == 6, .(permno, rebalance_date, adj_ret, adj_retx)]
  
  dt.jun <- dt.jun[, c("rebalance_date", "div_yield", "adj_ret", "adj_retx") := list(rebalance_date + years(1), adj_ret - adj_retx, NULL, NULL)]
  
  dt <- merge(dt, dt.jun, by = c("permno", "rebalance_date"), all.x = TRUE)

  return(dt)
}
