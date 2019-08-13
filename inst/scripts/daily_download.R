library(data.table)
library(lubridate)
library(RPostgres)

conn <- dbConnect(Postgres(),
                  host = 'wrds-pgdata.wharton.upenn.edu',
                  port = 9737,
                  dbname  = 'wrds',
                  user    = 'gsb25',
                  sslmode = 'require')

# Extract compustat data
SQL.comp <- paste("SELECT gvkey, datadate AS comp_date, pstkrv, pstkl, pstk, txditc, ceq, seq, bkvlps, upstk, at as assets, lt
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

comp[["pref_stock"]] <- ifelse(is.na(comp$pstkrv), comp$pstkl, comp$pstkrv)
comp[["pref_stock"]] <- ifelse(is.na(comp$pref_stock), comp$pstk, comp$pref_stock)
set(comp, which(is.na(comp[["pref_stock"]])), "pref_stock", 0)

set(comp, which(is.na(comp[["txditc"]])), "txditc", 0)

comp[["seq2"]] <- ifelse(is.na(comp$seq), comp$bkvlps + comp$upstk, comp$seq)
comp[["seq3"]] <- ifelse(is.na(comp$seq), comp$assets - comp$lt, comp$seq)

comp[["book_equity"]] <- comp$seq + comp$txditc - comp$pref_stock
set(comp, which(comp[["book_equity"]] <= 0), "book_equity", NaN)

comp[["book_equity2"]] <- comp$seq2 + comp$txditc - comp$pref_stock
set(comp, which(comp[["book_equity2"]] <= 0), "book_equity", NaN)

comp[["book_equity3"]] <- comp$seq3 + comp$txditc - comp$pref_stock
set(comp, which(comp[["book_equity3"]] <= 0), "book_equity", NaN)


# Get year and change date to end of month
comp[, year := year(comp_date)]
comp[, comp_date := end_of_month(comp_date)]

# Calculate rebalance date for compustat data
comp[, rebalance_date := ceiling_date(comp_date, "year") %m+% months(18) + years(1) - days(1)]

today <- as.Date(format(Sys.Date(), "%Y-%m-%d"))

# Set link end dates which are 'NA' to today's date
set(ccm, which(is.na(ccm[["link_end_date"]])), "link_end_date", today)

# Merge compustat and ccm by gvkey
comp.test <- merge(x = comp, y = ccm, by = "gvkey", all.x = TRUE, allow.cartesian = TRUE)

# Only keep data with rebalance dates between link start and end dates
comp.test <- comp.test[rebalance_date >= link_date & rebalance_date <= link_end_date]

