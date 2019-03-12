library(bsts)
library(chron)
library(data.table)
library(dplyr)
library(lubridate)
library(RPostgres)

#' Access the WRDS database
#'
#' This function accesses the WRDS database and returns
#' @export

# TODO: Add leverage ratios

extract.wrds <- function(username,
                         variables      = c("ME", "BE", "BE/ME", "A/ME", "A/BE", "OP", "INV", "E/P", "CF/P", "D/P"),
                         from           = as.Date("1963-07-31"),
                         to             = as.Date(paste0(format(Sys.Date() - years(1), "%Y"),"-12-31")),
                        # filter         = 'none', # Options will be raw data, fama french, liquidity. May allow selection of multiple
                         periodicity    = 'M', # Options will be D - daily, W - weekly, M - Monthly
                         rebalance.freq = 'A', # Options will be A - annually, S - semiannually, Q - quarterly
                         drop.excess    = T, # Boolean to drop extra variables extracted from WRDS database
                         preceding      = 60,
                         min.prec       = 0.4) {
cat("Extracting data...this could take a while")

  # Connect to wrds database
  wrds <- dbConnect(Postgres(),
                    host = 'wrds-pgdata.wharton.upenn.edu',
                    port = 9737,
                    dbname  = 'wrds',
                    user    = username,
                    sslmode = 'require')

  # TODO: Add more error checks
  stopifnot(any(periodicity %in% c('D', 'W', 'M')))
  stopifnot(any(rebalance.freq %in% c('Q', 'S', 'A')))



  if (rebalance.freq == 'A') {
    from <- max(ceiling_date(from %m-% months(6), "year") %m+% months(6), as.Date("1963-07-31"))
    to <- min(ceiling_date(to %m-% months(6), "year") %m+% months(6), as.Date(paste0(format(Sys.Date() - years(1), "%Y"),"-12-31")))
  } else if (rebalance.freq == 'S') {

  } else {

  }

  if (periodicity == 'D') {
    x <- getDailyData.WRDS(wrds, variables, from, to, filter, rebalance.freq, drop.excess, preceding, ceiling(preceding * min.prec))
  } else if (periodicity == 'W') {
    x <- getWeeklyData.WRDS(wrds, variables, from, to, filter, rebalance.freq, drop.excess, preceding, ceiling(preceding * min.prec))
  } else {
    x <- getMonthlyData.WRDS(wrds, variables, from, to, filter, rebalance.freq, drop.excess, preceding, ceiling(preceding * min.prec))
  }

  x$periodicity <- periodicity
  x$rebalance.freq <- rebalance.freq

  return(x)
}

getDailyData.WRDS <- function(conn, variables, from, to, filter, rebalance.freq, drop.excess, preceding) {
#   x <- list()
#
#   if (any(c("BE/ME", "OP", "CF/P") %in% variables) & !("BE" %in% variables)) {
#     variables <- c("BE", variables)
#   }
#
#   dict.options.comp <- list(BE     = c("pstkrv", "pstkl", "txditc", "seq"),
#                             OP     = c("ebitda", "xint AS interest_exp"),
#                             INV    = c("act AS assets",
#                                        "LAG(act, 1) as assets_prev OVER(PARTITION BY gvkey ORDER BY datadate)"),
#                             "E/P"  = "ib AS earnings",
#                             "CF/P" = c("act AS assets", "ib AS earnings", "txdc"))
#
#   options.comp <- unique(unlist(dict.options.comp[intersect(names(dict.options.comp), variables)]))
#
#   # Extract compustat data
#   SQL.comp <- paste("SELECT gvkey, datadate AS date, at, pstkl, txditc, pstkrv, seq, pstk,
#                             ib AS earnings, ebitda, xint AS interest_exp, act AS assets,
#                             LAG(act, 1) OVER(PARTITION BY gvkey ORDER BY datadate) as assets_prev, txdc
#                      FROM comp.funda
#                      WHERE indfmt = 'INDL'
#                      AND datafmt = 'STD'
#                      AND popsrc = 'D'
#                      AND consol = 'C'
#                      AND datadate >=", paste0("'", year(from) - 1, "-12-31'"),
#                     sep = " ")
#
#   res <- dbSendQuery(conn, SQL.comp)
#   comp <- as.data.table(dbFetch(res))
#   dbClearResult(res)
#
#   comp[, year := year(date)]
#   comp[, date := LastDayInMonth(date)]
#
#   if (any(c("BE/ME", "CF/P") %in% variables) & !("ME" %in% variables)) {
#     variables <- c("ME", variables)
#   }
#
#   # Obtain list of variables to extract from database
#   dict.options.crsp <- list(ME     = c("prc AS price", "shrout AS shares_out"),
#                             "E/P"  = "prc AS price")
#
#   options.crsp <- unique(unlist(dict.options.crsp[intersect(names(dict.options.crsp), variables)]))
#
#   options.crsp <- sapply(options.crsp, function(str) { paste0("a.", str) }, USE.NAMES = F)
#
#   crsp.database <- ifelse(periodicity == 'M', 'crsp.msf', 'crsp.dsf')
#
#   crsp.names <- ifelse(periodicity == 'M', 'crsp.msenames', 'crsp.dsenames')
#
#   # Extract CRSP data
#   SQL.crsp <- paste("SELECT a.permno, a.permco, a.date, b.shrcd AS share_code, b.exchcd AS exchange_code,
#                      a.ret, a.retx, a.shrout AS shares_out, a.prc AS price, a.vol AS volume
#                      FROM crsp.msfAS a
#                      LEFT join crsp.msenames AS b
#                      ON a.permno = b.permno
#                      AND b.namedt <= a.date
#                      AND a.date <= b.nameendt
#                      WHERE a.date BETWEEN", paste0("'", from %m-% months(preceding), "'"),
#                     "AND", paste0("'", to, "'"),
#                     "AND b.exchcd BETWEEN 1 AND 3
#                      AND a.hsiccd NOT between 6000 AND 6999",
#                     sep = " ")
#
#   res <- dbSendQuery(conn, SQL.crsp)
#   crsp <- data.table(dbFetch(res))
#   dbClearResult(res)
#
#   if (periodicity == 'M') {
#     crsp[, date := LastDayInMonth(date)]
#   }
#
#   delret.database <- ifelse(periodicity == 'M', 'crsp.msedelist', 'crsp.dsedelist')
#
#   SQL.delret <- paste("SELECT permno, dlret AS delist_ret, dlstdt AS date
#                        FROM", delret.database,
#                       sep = " ")
#
#   res <- dbSendQuery(conn, SQL.delret)
#   delret <- data.table(dbFetch(res))
#   dbClearResult(res)
#
#   if (periodicity == 'M') {
#     delret[, date := LastDayInMonth(date)]
#   }
#
#   crsp <- merge(crsp, delret, on = c("permno", "date"), all.x = TRUE)
#
#   set(crsp, which(is.na(crsp[["delist_ret"]])), "delist_ret", 0)
#
#   # Calculate returns from delisting returns
#   crsp[, adj_ret := (1 + ret) * (1 + delist_ret) - 1]
#
#   crsp[, adj_retx := (1 + retx) * (1 + delist_ret) - 1]
#
#   crsp[, delist_ret := NULL]
#
#   crsp <- crsp[order(date, permco)]
#
#   crsp[, offset_date := date %m-% months(6)]
#   crsp[, period := year(offset_date)]
#
#   ret <- crsp[[c("date", "permno", "adj_ret", "adj_retx")]]
#
#   # Extract CCM data
#   SQL.ccm <- paste("SELECT gvkey, lpermno AS permno, linktype, linkprim,
#                     linkdt AS link_date, linkenddt AS link_end_date
#                     FROM crsp.ccmxpf_linktable
#                     WHERE substr(linktype, 1, 1)='L'
#                     AND (linkprim ='C' OR linkprim='P')",
#                    sep = " ")
#
#   res <- dbSendQuery(conn, SQL.ccm)
#   ccm <- as.data.table(dbFetch(res))
#   dbClearResult(res)
#
#   # Apply filter here
#   if (filter) {
#     do.call(filter, c(comp, crsp))
#   }
#
#   set(ccm, which(is.na(ccm[["link_end_date"]])), "link_end_date", to)
#
#   comp <- merge(x = comp, y = ccm, by = "gvkey", all.x = TRUE, allow.cartesian = TRUE)
#
#   if (rebalance.freq == 'A') {
#     comp[, rebalance_date := ceiling_date(date, "year") %m+% months(6) - days(1)]
#     crsp[, end_period := ceiling_date(date %m-% months(6), "year") %m+% months(6)]
#
#   } else if (rebalance.freq == 'S') {
#
#   } else {
#
#   }
#
#
#   comp <- comp[rebalance_date >= link_date & rebalance_date <= link_end_date,]
#
#   raw <- list(crsp = crsp, comp = comp)
#
#   z <- getFundamentals(comp, crsp, variables)
#
#   # Select daily or monthly database
#   ind.database <- ifelse(periodicity == 'M', 'crsp.msi', 'crsp.dsi')
#
#   # Obtain returns for CRSP value-weighted index
#   SQL.ind = paste("SELECT date, vwretd AS ind_ret
#                    FROM", ind.database,
#                   "WHERE date BETWEEN", paste0("'", from %m-% months(preceding + 1), "'"),
#                   "AND", paste0("'", to, "'"),
#                   sep = " ")
#
#   res <- dbSendQuery(conn, SQL.ind)
#   ind <- as.data.table(dbFetch(res))
#   dbClearResult(res)
#
#   if (periodicity == 'M') {
#     ind[, date := LastDayInMonth(date)]
#   }
#
#   SQL.rf <- if (periodicity == 'M') {
#     paste("SELECT mcaldt AS date, tmyld AS risk_free
#            FROM crsp.tfz_mth_rf2
#            WHERE kytreasnox=2000061
#            AND mcaldt BETWEEN", paste0("'", from %m-% months(preceding), "'"),
#           "AND", paste0("'", to, "'"),
#           sep = " ")
#   } else {
#     paste("SELECT caldt AS date, tmyld AS risk_free
#            FROM crsp.tfz_dly_rf2
#            WHERE kytreasnox=2000061
#            AND caldt BETWEEN", paste0("'", from %m-% months(preceding), "'"),
#           "AND", paste0("'", to, "'"),
#           sep = " ")
#   }
#
#   res <- dbSendQuery(conn, SQL.rf)
#   rf <- as.data.table(dbFetch(res))
#   dbClearResult(res)
#
#   if (periodicity == 'M') {
#     rf[, date := LastDayInMonth(date)]
#   }
#
#   market.dat <- merge(ind, rf, by = "date", all.x = TRUE)
}

getWeeklyData.WRDS <- function(conn, variables, from, to, filter, rebalance.freq, drop.excess, preceding) {

}

getMonthlyData.WRDS <- function(conn, variables, from, to, filter, rebalance.freq, drop.excess, preceding) {
  x <- list()

  if (filter == "ff") {
    variables <- union(variables, c("BE", "ME", "BE/ME", "A/ME", "A/BE"))
  }

  if (any(c("BE/ME", "OP", "CF/P", "A/BE") %in% variables) & !("BE" %in% variables)) {
    variables <- c("BE", variables)
  }

  if (any(c("BE/ME", "CF/P", "A/ME") %in% variables) & !("ME" %in% variables)) {
    variables <- c("ME", variables)
  }

  # Obtain list of variables to extract from database
  dict.options.crsp <- list(ME     = c("a.prc AS price", "a.shrout AS shares_out"),
                            "E/P"  = "a.prc AS price")

  options.crsp <- paste(unique(unlist(dict.options.crsp[intersect(names(dict.options.crsp), variables)])), collapse = ', ')

  from.crsp <- paste0("'", from %m-% months(preceding), "'")
  to.crsp <- paste0("'", to, "'")

  # Extract CRSP data
  SQL.crsp <- paste("SELECT a.permno, a.permco, a.date, b.shrcd AS share_code, b.exchcd AS exchange_code,
                     a.ret, a.retx,", options.crsp, ", a.vol AS volume
                     FROM crsp.msf AS a
                     LEFT join crsp.msenames AS b
                     ON a.permno = b.permno
                     AND b.namedt <= a.date
                     AND a.date <= b.nameendt
                     WHERE a.date BETWEEN", from.crsp,
                    "AND", to.crsp,
                    "AND b.exchcd BETWEEN 1 AND 3
                     AND a.hsiccd NOT between 6000 AND 6999",
                    sep = " ")

  res <- dbSendQuery(conn, SQL.crsp)
  crsp <- data.table(dbFetch(res))
  dbClearResult(res)

  crsp[, date := LastDayInMonth(date)]

  SQL.delret <- "SELECT permno, dlret AS delist_ret, dlstdt AS date
                 FROM crsp.msedelist"

  res <- dbSendQuery(conn, SQL.delret)
  delret <- data.table(dbFetch(res))
  dbClearResult(res)

  delret[, date := LastDayInMonth(date)]

  crsp <- merge(crsp, delret, on = c("permno", "date"), all.x = TRUE)

  set(crsp, which(is.na(crsp[["delist_ret"]])), "delist_ret", 0)

  # Calculate returns from delisting returns
  crsp[, adj_ret := (1 + ret) * (1 + delist_ret) - 1]

  crsp[, adj_retx := (1 + retx) * (1 + delist_ret) - 1]

  crsp[, delist_ret := NULL]

  crsp <- crsp[order(date, permco)]

  if (rebalance.freq == 'A') {
    crsp[, rebalance_date := ceiling_date(date %m-% months(6), "year") %m+% months(6) - days(1)]
    comp <- getAnnualCompustat(conn, variables, from, to)
  } else if (rebalance.freq == 'S') {
    crsp[, rebalance_date := ceiling_date(date, "halfyear") - days(1)]
    comp <- getSemiAnnualCompustat(conn, variables, from, to)
  } else {
    crsp[, rebalance_date := ceiling_date(date , "quarter") - days(1)]
    comp <- getQuarterlyCompustat(conn, variables, from, to)
  }

  rets <- na.omit(crsp[, c("date", "permno", "adj_ret", "adj_retx", "rebalance_date")], cols = c("adj_ret", "adj_retx"))

  # Apply filter here
  # if (filter != "none") {
  #   crsp <- do.call(paste0("filter.", filter), crsp)
  # }

  raw <- list(crsp = crsp, comp = comp)

  z <- getFundamentals(comp, crsp, variables)

  from.ind <- paste0("'", from %m-% months(preceding + 1), "'")
  to.ind <- to.crsp

  # Obtain returns for CRSP value-weighted index
  SQL.ind = paste("SELECT date, vwretd AS ind_ret
                   FROM crsp.msi
                   WHERE date BETWEEN",  from.ind,
                  "AND", to.ind,
                  sep = " ")

  res <- dbSendQuery(conn, SQL.ind)
  ind <- as.data.table(dbFetch(res))
  dbClearResult(res)

  ind[, date := LastDayInMonth(date)]
  ind[, lag_ind_ret := lag(ind_ret, 1)]

  from.rf <- from.crsp
  to.rf <- to.crsp

  SQL.rf <- paste("SELECT caldt AS date, t90ret AS risk_free
                   FROM crsp.mcti
                   WHERE caldt BETWEEN", from.rf,
                  "AND", to.rf,
                  sep = " ")

  res <- dbSendQuery(conn, SQL.rf)
  rf <- as.data.table(dbFetch(res))
  dbClearResult(res)

  rf[, date := LastDayInMonth(date)]

  market.dt <- merge(ind[-1], rf, by = "date", all.x = TRUE)

  x$market.dt <- merge(rets, market.dat, by = "date", all.x = TRUE)

  return(x)
}

# database name is compq, report date is RDQ
getQuarterlyCompustat <- function(conn, variables, from, to) {

}

getSemiAnnualCompustat <- function(conn, variables, from, to) {

}

getAnnualCompustat <- function(conn, variables, from, to) {
  dict.options.comp <- list(BE     = c("pstkrv", "pstkl", "txditc", "seq"),
                            OP     = c("ebitda", "xint AS interest_exp"),
                            INV    = c("at AS assets",
                                       "LAG(at, 1) as assets_prev OVER(PARTITION BY gvkey ORDER BY datadate)"),
                            "A/BE" = "at AS assets",
                            "E/P"  = "ib AS earnings",
                            "CF/P" = c("at AS assets", "ib AS earnings", "txdc"))

  options.comp <- paste(unique(unlist(dict.options.comp[intersect(names(dict.options.comp), variables)])), collapse = ', ')

  from.comp <- paste0("'", year(from) - 1, "-12-31'")
  # Extract compustat data
  SQL.comp <- paste("SELECT gvkey, datadate AS date, at, pstkl, txditc, pstkrv, seq, pstk,
                            ib AS earnings, ebitda, xint AS interest_exp, at AS assets,
                            LAG(at, 1) OVER(PARTITION BY gvkey ORDER BY datadate) as assets_prev, txdc
                     FROM comp.funda
                     WHERE indfmt = 'INDL'
                     AND datafmt = 'STD'
                     AND popsrc = 'D'
                     AND consol = 'C'
                     AND datadate >=", from.comp,
                    sep = " ")

  res <- dbSendQuery(conn, SQL.comp)
  comp <- as.data.table(dbFetch(res))
  dbClearResult(res)

  comp[, year := year(date)]
  comp[, date := LastDayInMonth(date)]

  # Extract CCM data
  SQL.ccm <- paste("SELECT gvkey, lpermno AS permno, linktype, linkprim,
                    linkdt AS link_date, linkenddt AS link_end_date
                    FROM crsp.ccmxpf_linktable
                    WHERE substr(linktype, 1, 1)='L'
                    AND (linkprim ='C' OR linkprim='P')",
                   sep = " ")

  res <- dbSendQuery(conn, SQL.ccm)
  ccm <- as.data.table(dbFetch(res))
  dbClearResult(res)

  today <- as.Date(format(Sys.Date(), "%Y-%m-%d"))

  set(ccm, which(is.na(ccm[["link_end_date"]])), "link_end_date", today)

  comp <- merge(x = comp, y = ccm, by = "gvkey", all.x = TRUE, allow.cartesian = TRUE)

  comp[, rebalance_date := ceiling_date(date, "year") %m+% months(6) - days(1)]

  comp <- comp[rebalance_date >= link_date & rebalance_date <= link_end_date]

  return(comp)
}

getFundamentals <- function(comp, crsp, variables) {
  dict.comp <- c(BE  = "bookEquity",
                 OP  = "operatingProfitability",
                 INV = "investment")

  calls.comp <- as.vector(dict.comp[intersect(names(dict.comp), variables)])

  dict.crsp <- c(ME    = "marketEquity",
                 "D/P" = "dividendYield")

  calls.crsp <- as.vector(dict.crsp[intersect(names(dict.crsp), variables)])

  dict.merged <- c("BE/ME" = "bookToMarket",
                   "E/P" = "earningsToPrice",
                   "CF/P" = "cashFlowToPrice")

  calls.merged <- as.vector(dict.merged[intersect(names(dict.merged), variables)])

  # Get fundamental date from crsp and compustat databases
  for (i in 1:length(calls.comp)) {
    comp <- do.call(calls.comp[i], list(dt = comp))
  }

  for (i in 1:length(calls.crsp)) {
    crsp <- do.call(calls.crsp[i], list(dt = crsp))
  }

  # Merge crsp and compustat on rebalance dates
  ccm.rebal <- merge(crsp.rebal.date, comp, by = c("permno", "rebalance_date"))

  # Variables to keep
  var.comp <- list(BE     = c("book_equity", "log_book_equity"),
                   "A/BE" = c("assets_book", "log_assets_book"),
                   "A/ME" = "assets",
                   OP     = "oper_prof",
                   INV    = "investment",
                   "E/P"  = "earnings_price",
                   "CF/P" = c("assets", "depreciation", "deferred_tax", "equity_share", "earnings"))

  keep.comp <- as.vector(unlist(var.comp[intersect(names(var.comp), variables)]))

  var.crsp <- list(ME    = c("market_equity", "log_market_equity"),
                   "D/P" = "div_yield")

  keep.crsp <- as.vector(unlist(var.crsp[intersect(names(var.crsp), variables)]))

  var.names <- c(BE      = c("book_equity", "log_book_equity"),
                 ME      = c("market_equity", "log_market_equity"),
                 "BE/ME" = c("book_market", "log_book_market"),
                 "A/BE"  = c("assets_book", "log_assets_book"),
                 "A/ME"  = c("assets_market", "log_assets_market"),
                 OP      = "oper_prof",
                 INV     = "investment",
                 "D/P"   = "div_yield",
                 "E/P"   = "earnings_price",
                 "CF/P"  = "cf_price")

  keep <- as.vector(unlist(var.names[intersect(names(var.names), variables)]))

  keep <- c("date", "permno", "share_code", "exchange_code", "ret", "retx", keep)

  merged <- merged[keep]

  return(merged)
}
