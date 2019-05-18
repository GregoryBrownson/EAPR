#' Access the wrds database
#'
#' This function accesses the wrds database and returns
#'
#' @param username
#' @param src
#' @param variables
#' @param from
#' @param to
#' @param periodicity
#' @param rebalance.freq
#' @param preceding
#' @param min.prec
#'
#' @importFrom data.table data.table
#' @importFrom datetimeutils end_of_month
#' @importFrom lubridate ceiling_date '%m-%' '%m+%'
#' @importFrom RPostgres dbClearResult dbConnect dbFetch dbSendQuery Postgres
#'
#' @export

# TODO: Add leverage ratios

extract <- function(username,
                    src = "wrds",
                    variables      = c("ME", "BE", "BE/ME", "A/ME", "A/BE", "OP", "INV", "E/P", "CF/P", "D/P"),
                    from           = as.Date("1963-07-31"),
                    to             = as.Date(paste0(format(Sys.Date() - years(1), "%Y"),"-12-31")),
                    # filter         = 'none', # Options will be raw data, fama french, liquidity. May allow selection of multiple
                    periodicity    = 'M', # Options will be D - daily, W - weekly, M - Monthly
                    rebalance.freq = 'A', # Options will be A - annually, S - semiannually, Q - quarterly
                    drop.excess    = T, # Boolean to drop extra variables extracted from wrds database
                    preceding      = 60,
                    min.prec       = 0.4) {
  if (src == "wrds") {
    stop("Sorry, only extraction from the wrds database is implemented right now. Please set the 'src' variable to 'wrds'")
  }

  data <- do.call(paste('extract.', src, sep = ''),
                        list(username = usernamee,
                             variables = variables,
                             from = from,
                             to = to,
                             #filter = filter,
                             periodicity = periodicity,
                             rebalance.freq = rebalance.freq,
                             drop.excess = drop.excess,
                             preceding = preceding,
                             min.prec = min.prec))

  return(data)
}

extract.wrds <- function(username,
                         variables      = c("ME", "BE", "BE/ME", "A/ME", "A/BE", "OP", "INV", "E/P", "CF/P", "D/P"),
                         from           = as.Date("1963-07-31"),
                         to             = as.Date(paste0(format(Sys.Date() - years(1), "%Y"),"-12-31")),
                        # filter         = 'none', # Options will be raw data, fama french, liquidity. May allow selection of multiple
                         periodicity    = 'M', # Options will be D - daily, W - weekly, M - Monthly
                         rebalance.freq = 'A', # Options will be A - annually, S - semiannually, Q - quarterly
                         drop.excess    = T, # Boolean to drop extra variables extracted from wrds database
                         preceding      = 60,
                         min.prec       = 24) {
cat("Extracting data...this could take a while")

  # Connect to wrds database
  wrds <- dbConnect(Postgres(),
                    host = 'wrds-pgdata.wharton.upenn.edu',
                    port = 9737,
                    dbname  = 'wrds',
                    user    = username,
                    sslmode = 'require')

  valid.variables <- c("ME", "BE", "BE/ME", "A/ME", "A/BE", "OP", "INV", "E/P", "CF/P", "D/P")

  # Check for incorrect values
  stopifnot(any(periodicity %in% c('D', 'W', 'M')))
  stopifnot(any(rebalance.freq %in% c('Q', 'S', 'A')))

  # Adjust time frame based on rebalancing frequency
  if (rebalance.freq == 'A') {
    from <- max(ceiling_date(from %m-% months(6), "year") %m+% months(6), as.Date("1963-07-31"))
    to <- min(ceiling_date(to %m-% months(6), "year") %m+% months(6), as.Date(paste0(format(Sys.Date() - years(1), "%Y"),"-12-31")))
  } else if (rebalance.freq == 'S') {
    # TODO: Adjusted time frame for Semiannual rebalancing
    print('Semiannual rebalancing not implemented yet!')
  } else {
    # TODO: Adjusted time frame for Quarterly rebalancing
    print('Quarterly rebalancing not implemented yet!')
  }

  if (periodicity == 'D') {
    # x <- getDailyData.wrds(wrds, variables, from, to, filter, rebalance.freq, drop.excess, preceding, ceiling(preceding * min.prec))
    x <- list()
    print('Daily periodicity of returns not implemented yet!')
  } else if (periodicity == 'W') {
    # x <- getWeeklyData.wrds(wrds, variables, from, to, filter, rebalance.freq, drop.excess, preceding, ceiling(preceding * min.prec))
    x <- list()
    print('Weekly periodicity of returns not implemented yet!')
  } else {
    x <- getMonthlyData.wrds(wrds, variables, from, to, filter, rebalance.freq, drop.excess, preceding, ceiling(preceding * min.prec))
  }

  x$periodicity <- periodicity
  x$rebalance.freq <- rebalance.freq

  return(x)
}

getDailyData.wrds <- function(conn, variables, from, to, filter, rebalance.freq, drop.excess, preceding) {
  # TODO: Method to extract daily data
}

getWeeklyData.wrds <- function(conn, variables, from, to, filter, rebalance.freq, drop.excess, preceding) {
  # TODO: Method to extract weekly data
}

getMonthlyData.wrds <- function(conn, variables, from, to, filter, rebalance.freq, drop.excess, preceding) {
  x <- list()

  # # Check filter
  # if (filter == "ff") {
  #   variables <- union(variables, c("BE", "ME", "BE/ME", "A/ME", "A/BE"))
  # }

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
                     a.ret, a.retx,", options.crsp, ", a.vol AS volume,
                     CASE WHEN (a.hsiccd BETWEEN 6000 and 6999) THEN 1 ELSE 0 END AS is_financial
                     FROM crsp.msf AS a
                     LEFT join crsp.msenames AS b
                     ON a.permno = b.permno
                     AND b.namedt <= a.date
                     AND a.date <= b.nameendt
                     WHERE a.date BETWEEN", from.crsp,
                    "AND", to.crsp,
                    "AND b.exchcd BETWEEN 1 AND 3",
                    sep = " ")

  res <- dbSendQuery(conn, SQL.crsp)
  crsp <- data.table(dbFetch(res))
  dbClearResult(res)

  crsp[, date := end_of_month(date)]

  # Extract delisted return
  SQL.delret <- "SELECT permno, dlret AS delist_ret, dlstdt AS date
                 FROM crsp.msedelist"

  res <- dbSendQuery(conn, SQL.delret)
  delret <- data.table(dbFetch(res))
  dbClearResult(res)

  # Change date to end of month
  delret[, date := end_month(date)]

  # Merge crsp and delisted returns
  crsp <- merge(crsp, delret, on = c("permno", "date"), all.x = TRUE)

  set(crsp, which(is.na(crsp[["delist_ret"]])), "delist_ret", 0)

  # Calculate returns from delisting returns
  crsp[, adj_ret := (1 + ret) * (1 + delist_ret) - 1]

  crsp[, adj_retx := (1 + retx) * (1 + delist_ret) - 1]

  crsp[, delist_ret := NULL]

  crsp <- crsp[order(date, permco)]

  # Calculate rebalancing dates and extract compustat data
  # Note: Only extraction methods for annual compustat data are implemented
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

  # This line is for storing current data tables so further code can be tested without having to
  # query wrds again
  # raw <- list(crsp = crsp, comp = comp)

  # Obtain fundamentals
  z <- getFundamentals(comp, crsp, variables)

  # Get start and end dates for index time series
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

  # Change dates to end of month and add lagged index variable
  ind[, date := end_month(date)]
  ind[, lag_ind_ret := lag(ind_ret, 1)]

  # Start and end dates for risk-free rate time series
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

  # Change date to end of month
  rf[, date := end_month(date)]

  # Merge index data and risk-free rates
  market.dt <- merge(ind[-1], rf, by = "date", all.x = TRUE)

  # Merge individual stock returns with market data
  x$market.dt <- merge(rets, market.dt, by = "date", all.x = TRUE)

  class(x) <- 'eapr'

  return(x)
}

# database name is compq, report date is RDQ
getQuarterlyCompustat <- function(conn, variables, from, to) {
  # TODO: Implement extraction for quarterly compustat data
}

getSemiAnnualCompustat <- function(conn, variables, from, to) {
  # TODO: Implement extraction for semiannual computstat data
}

# Extracts annual compustat data
getAnnualCompustat <- function(conn, variables, from, to) {
  # Dictionary of variables which depend on Compustat data
  # Note: May want to use hashtable if this list continues to grow
  dict.options.comp <- list(BE     = c("pstkrv", "pstkl", "txditc", "seq"),
                            OP     = c("ebitda", "xint AS interest_exp"),
                            INV    = c("at AS assets",
                                       "LAG(at, 1) as assets_prev OVER(PARTITION BY gvkey ORDER BY datadate)"),
                            "A/BE" = "at AS assets",
                            "E/P"  = "ib AS earnings",
                            "CF/P" = c("at AS assets", "ib AS earnings", "txdc"))

  # Obtain names of data which need to be extracted from Compustat
  options.comp <- paste(unique(unlist(dict.options.comp[intersect(names(dict.options.comp), variables)])), collapse = ', ')

  # Compustat start date
  from.comp <- paste0("'", year(from) - 1, "-12-31'")

  # Extract compustat data
  SQL.comp <- paste("SELECT gvkey, datadate AS date,", options.comp,
                    "FROM comp.funda
                     WHERE indfmt = 'INDL'
                     AND datafmt = 'STD'
                     AND popsrc = 'D'
                     AND consol = 'C'
                     AND datadate >=", from.comp,
                    sep = " ")

  res <- dbSendQuery(conn, SQL.comp)
  comp <- as.data.table(dbFetch(res))
  dbClearResult(res)

  # Get year and change date to end of month
  comp[, year := year(date)]
  comp[, date := end_month(date)]

  # Extract CRSP-Compustat Merged data
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

  # Set link end dates which are 'NA' to today's date
  set(ccm, which(is.na(ccm[["link_end_date"]])), "link_end_date", today)

  # Merge compustat and ccm by gvkey
  comp <- merge(x = comp, y = ccm, by = "gvkey", all.x = TRUE, allow.cartesian = TRUE)

  # Calculate rebalance date for compustat data
  comp[, rebalance_date := ceiling_date(date, "year") %m+% months(6) - days(1)]

  # Only keep data with rebalance dates between link start and end dates
  comp <- comp[rebalance_date >= link_date & rebalance_date <= link_end_date]

  return(comp)
}

# Calls on variable functions and returns a merged data table
getFundamentals <- function(comp, crsp, variables) {
  cat("Calculating fundamentals...")

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
  merged <- merge(crsp, comp, by = c("permno", "date"))

  for (i in 1:length(calls.comp)) {
    merged <- do.call(calls.merged[i], list(dt = merged))
  }

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

  keep <- c("date", "permno", "share_code", "exchange_code", "ret", "adj_ret", "retx", "adj_retx", keep)

  merged <- merged[, keep]

  return(merged)
}

getTechnicals <- function(x) {
  # TODO: Implement method to call on functions to compute technical variables
}
