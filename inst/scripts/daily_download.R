library(data.table)
library(lubridate)
library(RPostgres)

username = "gsb25" # WRDS USERNAME

from <- as.Date("2016-1-1")

conn <- dbConnect(Postgres(),
                  host = 'wrds-pgdata.wharton.upenn.edu',
                  port = 9737,
                  dbname  = 'wrds',
                  user    = username,
                  sslmode = 'require')

from.crsp <- paste0("'", from, "'")
to.crsp <- paste0("'", to.crsp, "'")

# Extract CRSP data
SQL.crsp <- paste("SELECT a.permno, a.permco, a.date, b.shrcd AS share_code, b.exchcd AS exchange_code,
                   a.ret, a.retx, a.prc AS price, a.vol AS volume, a.shrout AS shares_out,
                   CASE WHEN (a.hsiccd BETWEEN 6000 and 6999) THEN 1 ELSE 0 END AS is_financial
                   FROM crsp.dsf AS a
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

# Extract delisted return
SQL.delret <- "SELECT permno, dlret AS delist_ret, dlstdt AS date
                 FROM crsp.dsedelist"

res <- dbSendQuery(conn, SQL.delret)
delret <- data.table(dbFetch(res))
dbClearResult(res)

# Merge crsp and delisted returns
crsp <- merge(crsp, delret, on = c("permno", "date"), all.x = TRUE)

set(crsp, which(is.na(crsp[["delist_ret"]])), "delist_ret", 0)

# Calculate returns from delisting returns
crsp[, adj_ret := (1 + ret) * (1 + delist_ret) - 1]

crsp[, adj_retx := (1 + retx) * (1 + delist_ret) - 1]

crsp[, delist_ret := NULL]

crsp <- crsp[order(date, permco)]

rets <- na.omit(crsp[, c("date", "permno", "adj_ret", "adj_retx", "rebalance_date")], cols = c("adj_ret", "adj_retx"))

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
