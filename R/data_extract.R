#' Access the wrds database
#' 
#' This function accesses the wrds database and returns
#' 
#' @importFrom data.table data.table as.data.table
#' @importFrom datetimeutils end_of_month
#' @importFrom dplyr full_join group_by mutate '%>%' ungroup
#' @importFrom parallel clusterExport clusterEvalQ detectCores parLapply
#' @importFrom purrr reduce
#' @importFrom lubridate ceiling_date '%m-%' '%m+%' year days years
#' @importFrom RPostgres dbClearResult dbConnect dbFetch dbSendQuery Postgres
#'
#' @param username username for database
#' @param src source of database
#' @param fundamental.var fundamental variables to be computed/extracted
#' @param technical.var technical variables to be computed/extracted
#' @param from starting date
#' @param to end date
#' @param periodicity frequency of the data
#' @param rebalance.freq how often portfolios are rebalanced
#' @param drop.excess Drop variables used to calculate fundamentals and technicals
#' @param preceding number of preceding periods to consider for technical variables
#' @param min.prec minimum number of preceding periods necessary to compute technical variables
#'
#' @export

# TODO: Add leverage ratios

extract <- function(username,
                    src = "wrds",
                    fundamental.var = c("ME", "BE", "BE/ME", "A/ME", "A/BE", "OP", "INV", "E/P", "CF/P", "D/P"),
                    technical.var   = c("PreBeta"),
                    from            = as.Date("1963-07-31"),
                    to              = as.Date(paste0(format(Sys.Date() - years(1), "%Y"),"-12-31")),
                    # filter          = 'none', # Options will be raw data, fama french, liquidity. May allow selection of multiple
                    periodicity     = 'M', # Options will be D - daily, W - weekly, M - Monthly
                    rebalance.freq  = 'A', # Options will be A - annually, S - semiannually, Q - quarterly
                    drop.excess     = T, # Boolean to drop extra variables extracted from wrds database
                    preceding       = 60,
                    min.prec        = 0.4) {
  if (src != "wrds") {
    stop("Sorry, only extraction from the wrds database is implemented right now. Please set the 'src' variable to 'wrds'")
  }

  data <- do.call(paste('extract.', src, sep = ''),
                  list(username = username,
                       fundamental.var = fundamental.var,
                       technical.var = technical.var,
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
                         fundamental.var = c("ME", "BE", "BE/ME", "A/ME", "A/BE", "OP", "INV", "E/P", "CF/P", "D/P"),
                         technical.var   = c("PreBeta", "PostBeta"),
                         from            = as.Date("1963-07-31"),
                         to              = as.Date(paste0(format(Sys.Date() - years(1), "%Y"),"-12-31")),
                         # filter          = 'none', # Options will be raw data, fama french, liquidity. May allow selection of multiple
                         periodicity     = 'M', # Options will be D - daily, W - weekly, M - Monthly
                         rebalance.freq  = 'A', # Options will be A - annually, S - semiannually, Q - quarterly
                         drop.excess     = T, # Boolean to drop extra fundamental.var extracted from wrds database
                         preceding       = 60,
                         min.prec        = 24) {
cat("Extracting data...this could take a while\n")

  # Connect to wrds database
  conn <- dbConnect(Postgres(),
                    host = 'wrds-pgdata.wharton.upenn.edu',
                    port = 9737,
                    dbname  = 'wrds',
                    user    = username,
                    sslmode = 'require')

  valid.fundamental.var <- c("ME", "BE", "BE/ME", "A/ME", "A/BE", "OP", "INV", "E/P", "CF/P", "D/P")

  # Check for incorrect values
  stopifnot(any(periodicity %in% c('D', 'W', 'M')))
  stopifnot(any(rebalance.freq %in% c('Q', 'S', 'A')))

  # Adjust time frame based on rebalancing frequency
  if (rebalance.freq == 'A') {
    from <- max(ceiling_date(from %m-% months(6), "year") %m-% months(6), as.Date("1963-07-01"))
    to <- min(ceiling_date(to %m-% months(6), "year") - days(1), as.Date(paste0(format(Sys.Date() - years(1), "%Y"),"-07-01")))
  } else if (rebalance.freq == 'S') {
    # TODO: Adjusted time frame for Semiannual rebalancing
    print('Semiannual rebalancing not implemented yet!')
  } else {
    # TODO: Adjusted time frame for Quarterly rebalancing
    print('Quarterly rebalancing not implemented yet!')
  }

  if (periodicity == 'D') {
    # x <- getDailyData.wrds(wrds, fundamental.var, technical.var, from, to, filter, rebalance.freq, drop.excess, preceding, ceiling(preceding * min.prec))
    x <- list()
    print('Daily periodicity of returns not implemented yet!')
  } else if (periodicity == 'W') {
    # x <- getWeeklyData.wrds(wrds, fundamental.var, technical.var, from, to, filter, rebalance.freq, drop.excess, preceding, ceiling(preceding * min.prec))
    x <- list()
    print('Weekly periodicity of returns not implemented yet!')
  } else {
    x <- getMonthlyData.wrds(conn, fundamental.var, technical.var, from, to, filter, rebalance.freq, drop.excess, preceding, ceiling(preceding * min.prec))
  }

  x$periodicity <- periodicity
  x$rebalance.freq <- rebalance.freq

  return(x)
}

getDailyData.wrds <- function(conn, fundamental.var, technical.var, from, to, filter, rebalance.freq, drop.excess, preceding, min.prec) {
  # TODO: Method to extract daily data
  stop("Daily data not yet supported!")
}

getWeeklyData.wrds <- function(conn, fundamental.var, technical.var, from, to, filter, rebalance.freq, drop.excess, preceding, min.prec) {
  # TODO: Method to extract weekly data
  stop("Weekly data not yet supported!")
}

getMonthlyData.wrds <- function(conn, fundamental.var, technical.var, from, to, filter, rebalance.freq, drop.excess, preceding, min.prec) {
  x <- list()

  # # Check filter
  # if (filter == "ff") {
  #   fundamental.var <- union(fundamental.var, c("BE", "ME", "BE/ME", "A/ME", "A/BE"))
  # }

  if (any(c("BE/ME", "OP", "CF/P", "A/BE") %in% fundamental.var) & !("BE" %in% fundamental.var)) {
    fundamental.var <- c("BE", fundamental.var)
  }

  if (any(c("BE/ME", "CF/P", "A/ME") %in% fundamental.var) & !("ME" %in% fundamental.var)) {
    fundamental.var <- c("ME", fundamental.var)
  }

  # Obtain list of variables to extract from database
  dict.options.crsp <- list(ME     = c("a.prc AS price", "a.shrout AS shares_out"),
                            "E/P"  = "a.prc AS price")

  options.crsp <- paste(unique(unlist(dict.options.crsp[intersect(names(dict.options.crsp), fundamental.var)])), collapse = ', ')

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

  crsp = na.omit(crsp, cols = c("ret", "retx"))

  crsp[, date := end_of_month(date)]

  # Extract delisted return
  SQL.delret <- "SELECT permno, dlret AS delist_ret, dlstdt AS date
                 FROM crsp.msedelist"

  res <- dbSendQuery(conn, SQL.delret)
  delret <- data.table(dbFetch(res))
  dbClearResult(res)

  # Change date to end of month
  delret[, date := end_of_month(date)]

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
    comp <- getAnnualCompustat(conn, fundamental.var, from, to)
  } else if (rebalance.freq == 'S') {
    crsp[, rebalance_date := ceiling_date(date, "halfyear") - days(1)]
    comp <- getSemiAnnualCompustat(conn, fundamental.var, from, to)
  } else {
    crsp[, rebalance_date := ceiling_date(date , "quarter") - days(1)]
    comp <- getQuarterlyCompustat(conn, fundamental.var, from, to)
  }

  # Apply filter here
  # if (filter != "none") {
  #   crsp <- do.call(paste0("filter.", filter), crsp)
  # }

  # This line is for storing current data tables so further code can be tested without having to
  # query wrds again
  # raw <- list(crsp = crsp, comp = comp)
  
  rets <- na.omit(crsp[, c("date", "permno", "adj_ret", "adj_retx", "rebalance_date")], cols = c("adj_ret", "adj_retx"))
  
  # Obtain
  x <- getFundamentals(comp, crsp, fundamental.var, rebalance.freq)

  x$ccm <- x$ccm[x$ccm$date >= from]
  
  x$ccm[, c("period_ret", "period_adj_ret", "period_retx", "period_adj_retx")
          := list(prod(1 + ret) - 1, prod(1 + adj_ret) - 1, prod(1 + retx) - 1, prod(1 + adj_retx) - 1),
        by = .(rebalance_date, permno)]
  
  # Split data by rebalance date for cross-sectional regressions. Note: Could wait to do this in cross_sectional_regression.R 

  # Get start and end dates for index time series
  from.ind <- paste0("'", from %m-% months(preceding + 1), "'")
  to.ind <- to.crsp

  # Obtain returns for CRSP value-weighted index
  SQL.ind = paste("SELECT date, vwretd AS ind_ret, LAG(vwretd, 1) OVER(ORDER BY date) as lag_ind_ret
                   FROM crsp.msi
                   WHERE date BETWEEN",  from.ind,
                  "AND", to.ind,
                  sep = " ")

  res <- dbSendQuery(conn, SQL.ind)
  ind <- as.data.table(dbFetch(res))[-1]
  dbClearResult(res)

  # Change dates to end of month and add lagged index variable
  ind[, date := end_of_month(date)]

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
  rf[, date := end_of_month(date)]

  # Merge index data and risk-free rates
  market.dt <- merge(ind, rf, by = "date", all.x = TRUE)

  # Merge individual stock returns with market data
  x$market.dt <- merge(rets, market.dt, by = "date", all.x = TRUE)

  class(x) <- "eapr"

  return(x)
}

# database name is compq, report date is RDQ
getQuarterlyCompustat <- function(conn, fundamental.var, from, to) {
  # TODO: Implement extraction for quarterly compustat data
}

getSemiAnnualCompustat <- function(conn, fundamental.var, from, to) {
  # TODO: Implement extraction for semiannual computstat data
}

# Extracts annual compustat data
getAnnualCompustat <- function(conn, fundamental.var, from, to) {
  # Dictionary of variables which depend on Compustat data
  # Note: May want to use hashtable as this list continues to grow
  dict.options.comp <- list(BE     = c("pstkrv", "pstkl", "pstk", "txditc", "seq", "bkvlps", "upstk", "at as assets", "lt"),
                            OP     = c("ebitda", "xint AS interest_exp"),
                            INV    = c("at AS assets",
                                       "LAG(at, 1) OVER(PARTITION BY gvkey ORDER BY datadate) as assets_prev "),
                            "A/BE" = "at AS assets",
                            "E/P"  = "ib AS earnings",
                            "CF/P" = c("at AS assets", "ib AS earnings", "txdc as deferred_tax", "dp as depreciation"))

  # Obtain names of data which need to be extracted from Compustat
  options.comp <- paste(unique(unlist(dict.options.comp[intersect(names(dict.options.comp), fundamental.var)])), collapse = ', ')

  if (length(options.comp) > 0) {
    options.comp <- paste(',', options.comp)
  }
  
  # Compustat start date
  if ("INV" %in% fundamental.var) {
    from.comp <- paste0("'", year(from) - 2, "-12-31'")
  } else {
    from.comp <- paste0("'", year(from) - 1, "-12-31'")
  }

  # Extract compustat data
  SQL.comp <- paste("SELECT gvkey, datadate AS comp_date", options.comp,
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

  if ("INV" %in% fundamental.var) {
    comp <- comp[-1]
  }

  # Get year and change date to end of month
  comp[, year := year(comp_date)]
  comp[, comp_date := end_of_month(comp_date)]

  # Calculate rebalance date for compustat data
  comp[, rebalance_date := ceiling_date(comp_date, "year") %m+% months(18) + years(1) - days(1)]

  # Extract CRSP-Compustat Merged data
  SQL.ccm <- paste("SELECT gvkey, lpermno AS permno, linktype, linkprim,
                    linkdt AS link_date, linkenddt AS link_end_date
                    FROM crsp.ccmxpf_linktable
                    WHERE (linktype='LU' OR linktype='LC')
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
  
  # Only keep data with rebalance dates between link start and end dates
  comp <- comp[rebalance_date >= link_date & rebalance_date <= link_end_date]
  
  comp <- comp %>% group_by(permno, rebalance_date) %>%
    mutate(max_date = max(comp_date)) %>%
    ungroup() %>%
    as.data.table()
  
  # Only keep data related to later date
  comp <- comp[comp_date == max_date]
  
  comp[, c("comp_date", "max_date") := NULL]

  return(comp)
}

# Calls on variable functions and returns a merged data table
getFundamentals <- function(comp, crsp, variables, rebalance.freq) {
  cat("Calculating fundamentals...\n")

  dict.comp <- c(BE  = "bookEquity",
                 OP  = "operatingProfitability",
                 INV = "investment",
                 "A/BE" = "assetToBook")

  calls.comp <- as.vector(dict.comp[intersect(names(dict.comp), variables)])

  dict.crsp <- c(ME    = "marketEquity",
                 "D/P" = "dividendYield")

  calls.crsp <- as.vector(dict.crsp[intersect(names(dict.crsp), variables)])

  dict.merged <- c("BE/ME" = "bookToMarket",
                   "E/P"   = "earningsToPrice",
                   "CF/P"  = "cashFlowToPrice",
                   "A/ME"  = "assetToMarket")

  calls.merged <- as.vector(dict.merged[intersect(names(dict.merged), variables)])
  
  # Get fundamental date from crsp and compustat databases
  for (i in 1:length(calls.comp)) {
    comp <- do.call(calls.comp[i], list(dt = comp))
  }
  
  for (i in 1:length(calls.crsp)) {
    crsp <- do.call(calls.crsp[i], list(dt = crsp))
  }
  
  # Merge crsp and compustat on rebalance dates
  merged <- merge(crsp, comp, by = c("permno", "rebalance_date"), all = FALSE)

  for (i in 1:length(calls.merged)) {
    merged <- do.call(calls.merged[i], list(dt = merged))
  }

  # Variables to keep
  # var.comp <- list(BE     = c("book_equity", "log_book_equity"),
  #                  "A/BE" = c("assets_book", "log_assets_book"),
  #                  "A/ME" = "assets",
  #                  OP     = "oper_prof",
  #                  INV    = "investment",
  #                  "E/P"  = "earnings",
  #                  "CF/P" = c("assets", "depreciation", "deferred_tax", "equity_share", "earnings"))
  #
  # keep.comp <- as.vector(unique(unlist(var.comp[intersect(names(var.comp), variables)])))
  #
  # var.crsp <- list(ME    = c("market_equity", "log_market_equity"),
  #                  "D/P" = "div_yield")
  #
  # keep.crsp <- as.vector(unlist(var.crsp[intersect(names(var.crsp), variables)]))

  var.names <- list(BE      = c("book_equity", "log_book_equity"),
                    ME      = c("market_equity", "log_market_equity", "rDate_market_equity", "log_rDate_market_equity"),
                    "BE/ME" = c("book_market", "log_book_market"),
                    "A/BE"  = c("asset_book", "log_asset_book"),
                    "A/ME"  = c("asset_market", "log_asset_market"),
                    OP      = "oper_prof",
                    INV     = "investment",
                    "D/P"   = "div_yield",
                    "E/P"   = c("earnings_price", "earnings_price_indicator", "positive_earnings_price"),
                    "CF/P"  = "cf_price")

  if (rebalance.freq == "A") {
    var.names$ME <- c(var.names$ME, "dec_market_equity", "log_dec_market_equity")
  }
  
  keep <- as.vector(unlist(var.names[intersect(names(var.names), variables)]))

  keep <- c("date", "rebalance_date", "permno", "share_code", "exchange_code", "is_financial", "ret", "adj_ret", "retx", "adj_retx", "price", keep)

  merged <- merged[, ..keep]

  return(list(ccm = merged[!duplicated(merged)]))
}

getTechnicals <- function(x, variables, preceding, min.prec) {
  # TODO: Implement method to call on functions to compute technical variables
  cat("Calculating fundamentals...\n")

  split.market.dt <- split(x$market.dt, x$market.dt$permno)

  func.dict <- c(PreBeta  = "preRankBeta",
                 PostBeta = "postRankBeta")

  func.calls <- as.vector(func.dict[intersect(names(func.dict), variables)])

  supp.func.dict <- c(PreBeta = "computeDimsonBeta",
                      PostBeta = "computeDimsonBeta")

  supp.func.calls <- as.vector(unique(func.dict[intersect(names(func.dict), variables)]))

  technicals <- unlist(lapply(split.market.dt, function(x, funcs, preceding, min.prec) {
                                dat <- lapply(funcs, function(f, dt, preceding, min.prec) {
                                                do.call(f, list(dt, preceding, min.prec))
                                              },
                                              dt = x,
                                              preceding = preceding,
                                              min.prec  = min.prec)
                                reduce(dat, full_join, by = "date")
                              },
                              funcs = func.calls,
                              preceding  = preceding,
                              min.prec   = min.prec))

    cl <- makeCluster(as.integer(max(detectCores() * 3 / 4, 1)))

    clusterEvalQ(cl, library(lubridate))
    clusterEvalQ(cl, library(stats))
    clusterEvalQ(cl, library(RobStatTM))
    clusterEvalQ(cl, library(zoo))

    clusterExport(cl, c(func.calls, supp.func.calls,  "data.table", "reduce", "full_join"))

    technicals <- parLapply(cl, split.market.dt, function(x, funcs, preceding, min.prec) {
                              dat <- lapply(funcs, function(f, dt, preceding, min.prec) {
                                              do.call(f, list(dt, preceding, min.prec))
                                            },
                                            dt = x,
                                            preceding = preceding,
                                            min.prec  = min.prec)
                              reduce(dat, full_join, by = "date")
                            },
                            funcs = func.calls,
                            preceding  = preceding,
                            min.prec   = min.prec)
}
