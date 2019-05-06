# Implementations of fundamental variable computations. These functions will be internal and should NOT be
# exported.

bookEquity <- function(dt) {
  # comp
  # Dependencies: pstkrv, pstkl, txditc, seq

  dt[["pref_stock"]] <- ifelse(is.na(dt$pstkrv), dt$pstkl, dt$pstkrv)
  dt[["pref_stock"]] <- ifelse(is.na(dt$pref_stock), dt$pstk, dt$pref_stock)
  set(dt, which(is.na(dt[["pref_stock"]])), "pref_stock", 0)

  set(dt, which(is.na(dt[["txditc"]])), "txditc", 0)

  dt[["book_equity"]] <- dt$seq + dt$txditc - dt$pref_stock
  set(dt, which(dt[["book_equity"]] <= 0), "book_equity", NaN)

  dt[["log_book_equity"]] <- log(dt$book_equity)

  dt[, c("pstkrv", "pstkl", "txditc", "seq") := NULL]

  return(dt)
}

marketEquity <- function(dt) {
  # crsp
  # Dependencies: prc, shrout
  dt[["market_equity"]] <- abs(dt$price) * dt$shares_out

  dt[["log_market_equity"]] <- log(dt$market_equity)

  dt.grouped <- group_by(dt, date, permco)

  max.me <- as.data.table(summarize(dt.grouped,
                                    sum_me = sum(market_equity),
                                    market_equity = max(market_equity)))

  dt <- merge(dt, max.me, by = c("date", "permco", "market_equity"))

  dt[, ':='(market_equity = sum_me,
            sum_me        = NULL)]

  return(dt)
}

bookToMarket <- function(dt) {
  # merged
  dt[["book_market"]] <- dt$book_equity / dt$market_equity

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

assetsToMarket <- function(dt) {
  # merged
  # Dependencies: at

  dt[["asset_market"]] <- dt$assets / dt$market_equity

  dt[["log_asset_market"]] <- log(dt$asset_market)

  return(dt)
}

earningsToPrice <- function(dt) {
  # merged
  # Dependencies: ib, prc
  dt[["earnings_price"]] <- dt$earnings / abs(dt$price)

  return(dt)
}

# comp
operatingProfitability <- function(dt) {
  # comp
  # dependencies: ebitda, xint
  dt[["oper_prof"]] <- (dt$ebitda - dt$interest_exp) / dt$book_equity

  dt[, c("ebitda", "interest_exp") := NULL]

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
  dt[["equity_share"]] <- dt$market_equity / (dt$assets - dt$book_equity + dt$market_equity)

  dt[["cf_price"]] <- dt$earnings + dt$equity_share * dt$depreciation + dt$deferred_tax

  dt[, c("assets", "depreciation", "deferred_tax", "equity_share", "earnings") := NULL]

  return(dt)
}

dividendYield <- function(dt) {
  # crsp
  dt[["div_yield"]] <- dt$adj_ret - dt$adj_retx

  return(dt)
}
