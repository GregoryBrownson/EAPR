library(data.table)
library(devtools)
library(ggplot2)
library(lmtest)
library(purrr)
library(RobStatTM)
library(sandwich)
library(xts)

devtools::load_all(".")

eapr <- EAPR::extract("gsb25")

dat.filtered <- EAPR::filter.ff92(eapr)

dat.11_1998 <- dat.filtered[date == as.Date("1998-11-30"), .(date, permno, adj_ret, adj_retx, rDate_market_equity, log_rDate_market_equity)]

any(duplicated(dat.filtered$ccm))

data.split <- split(dat.filtered$ccm, dat.filtered$ccm$date)

## SIZE

fit.classic.size <- lapply(data.split, function(x) {
                             x <- na.omit(x, c("adj_ret", "log_rDate_market_equity"))
                             unname(coef(lm(adj_ret * 100 ~ log_rDate_market_equity, data = x)))[2]
                           })

fit.robust.size <- lapply(data.split, function(x) {
                            x <- na.omit(x, c("adj_ret", "log_rDate_market_equity"))
                            control <- lmrobdet.control(efficiency = 0.99)
                            unname(coef(lmrobdetMM(adj_ret * 100 ~ log_rDate_market_equity, data = x, control = control)))[2]
                          })

dates <- as.Date(names(unlist(fit.classic.size)))

fit.size.dt <- data.table(date = dates, LS = unlist(fit.classic.size), ROBUST = unlist(fit.robust.size))

fit.size.dt.melted <- melt(fit.size.dt[date >= as.Date("1963-07-01")], id.vars = "date")
        
fit.size.jan.dt.melted <- fit.size.dt.melted[month(date) == 1 & year(date) >= 1964]

fit.classic.size.dt <- data.table(date = as.Date(dates), beta = unlist(fit.classic.size))

fit.classic.size.dt <- fit.classic.size.dt[date >= as.Date("1963-07-01") & year(date) <= 1990]

ggplot() +
        geom_hline(yintercept = 0) +
        geom_line(data = fit.size.dt.melted[year(date) <= 1990], aes(x = date, y = value), col = "dodgerblue2") +
        geom_point(data = fit.size.jan.dt.melted[year(date) <= 1990], aes(x = date, y = value), size = 1) +
        facet_grid(rows = vars(variable), switch = "both") +
        # geom_hline(yintercept = -3, data = subset(fit.size.dt.melted, variable == "LS")) +
        # geom_hline(yintercept = -4, data = subset(fit.size.dt.melted, variable == "ROBUST")) +
        ggtitle("Time Series of Size Slopes") +
        xlab("Date") + 
        scale_y_continuous(position = "right", limits = c(-5, 5)) +
        theme_bw() +
        theme(axis.title.y = element_blank())

fit <- lm(beta ~ 1, fit.classic.size.dt)

size.test <- coeftest(fit, vcov. = NeweyWest(fit, lag = 6, prewhite = FALSE))

v = lrvar(fit.classic.size.dt$beta, type = "Newey-West")

mu = mean(fit.classic.size.dt$beta)

mu / sqrt(v)

## BOOK-TO-MARKET

fit.classic.book_market <- lapply(data.split, function(x) {
        x <- na.omit(x, c("adj_ret", "log_book_market"))
        unname(coef(lm(adj_ret * 100 ~ log_book_market, data = x)))[2]
})

fit.robust.book_market <- lapply(data.split, function(x) {
        x <- na.omit(x, c("adj_ret", "log_book_market"))
        control <- lmrobdet.control(efficiency = 0.99)
        unname(coef(lmrobdetMM(adj_ret * 100 ~ log_book_market, data = x, control = control)))[2]
})

dates <- as.Date(names(fit.classic.book_market))

fit.book_market.dt <- data.table(date = dates, LS = unlist(fit.classic.book_market), ROBUST = unlist(fit.robust.book_market))

fit.book_market.dt.melted <- melt(fit.book_market.dt[date >= as.Date("1963-07-01")], id.vars = "date")

fit.book_market.jan.dt.melted <- fit.book_market.dt.melted[month(date) == 1 & year(date) >= 1964]

fit.classic.book_market.dt <- data.table(date = as.Date(dates), beta = unlist(fit.classic.book_market))

fit.classic.book_market.dt <- fit.classic.book_market.dt[year(date) >= 1980 & year(date) <= 2015]

ggplot() +
        geom_line(data = fit.book_market.dt.melted[year(date) >= 1980 & year(date) <= 2015], aes(x = date, y = value), col = "blue") +
        geom_point(data = fit.book_market.jan.dt.melted[year(date) >= 1980 & year(date) <= 2015], aes(x = date, y = value), size = 1) +
        facet_grid(rows = vars(variable)) +
        geom_hline(yintercept = 0) +
        ggtitle("Time Series of Book-to-Market Slopes") +
        xlab("Date") + 
        ylab("Slope") +
        theme_bw()

fit <- lm(beta ~ 1, fit.classic.book_market.dt)

book_market.test <- coeftest(fit, vcov. = NeweyWest(fit, lag = 6, prewhite = FALSE))

v = lrvar(fit.classic.size.dt$beta, type = "Newey-West")

mu = mean(fit.classic.size.dt$beta)

mu / sqrt(v)

## Earnings-to-Price

fit.classic.earnings_price <- lapply(data.split, function(x) {
        x <- na.omit(x, c("adj_ret", "earnings_price_indicator", "positive_earnings_price"))
        unname(coef(lm(adj_ret * 100 ~ earnings_price_indicator + positive_earnings_price, data = x)))
})

# fit.robust.earnings_price <- lapply(data.split, function(x) {
#         x <- na.omit(x, c("adj_ret", "earnings_price_indicator", "positive_earnings_price"))
#         control <- lmrobdet.control(efficiency = 0.99)
#         unname(coef(lmrobdetMM(adj_ret * 100 ~ earnings_price_indicator + positive_earnings_price, data = x, control = control)))
# })

for (i in 1:length(data.split)) {
        print(i)
        x <- na.omit(data.split[[i]], c("adj_ret", "earnings_price_indicator", "positive_earnings_price"))
        control <- lmrobdet.control(efficiency = 0.99)
        unname(coef(lmrobdetMM(adj_ret * 100 ~ earnings_price_indicator, data = x, control = control)))
}

dates <- as.Date(names(fit.classic.earnings_price))

fit.classic.earnings_price.dt <- data.table(matrix(unlist(fit.classic.earnings_price), ncol = 3, byrow = TRUE))

colnames(fit.classic.earnings_price.dt) <- c("beta_0", "beta_1", "beta_2")

fit.classic.earnings_price.dt <- cbind(data.table(date = dates), fit.classic.earnings_price.dt)[year(date) <= 2015]

fit.classic.earnings_price.jan.dt <- fit.classic.earnings_price.dt[month(date) == 1]

ggplot() +
        geom_line(data = fit.classic.earnings_price.dt, aes(x = date, y = beta_1), col = "dodgerblue2") +
        geom_point(data = fit.classic.earnings_price.jan.dt, aes(x = date, y = beta_1), size = 1) +
        ggtitle("Time Series of EP-dummy Slopes") +
        xlab("Date") + 
        ylab("Slope") +
        theme_bw()

fit <- lm(beta_1 ~ 1, fit.classic.earnings_price.dt)

earnings_price.test <- coeftest(fit, vcov. = NeweyWest(fit, lag = 6, prewhite = FALSE))

ggplot() +
        geom_line(data = fit.classic.earnings_price.dt, aes(x = date, y = beta_2), col = "dodgerblue2") +
        geom_point(data = fit.classic.earnings_price.jan.dt, aes(x = date, y = beta_2), size = 1) +
        ggtitle("Time Series of Positive Earnings-to-Price Slopes") +
        xlab("Date") + 
        ylab("Slope") +
        theme_bw()

fit <- lm(beta_2 ~ 1, fit.classic.earnings_price.dt)

earnings_price.test2 <- coeftest(fit, vcov. = NeweyWest(fit, lag = 6, prewhite = FALSE))

## Chris Data ##

library(data.table)

dat <- fread("/home/gsb25/Desktop/EAPR_additional/data/ccm_sizeport3_betaport3_port.csv")

dat.nfin <- dat[isFinancial == 0][order(PERMNO, DATE)]

dat.size <- dat.nfin[, .(DATE, PERMNO, retdadjpct, ME, logme, juneme, junelogme, be, beme, logbeme, epdummy, eplus2p, e2p, fyear)]

dat.size[, c("PERMNO", "retdadjpct", "ME", "logme", "juneme", "junelogme", "be", "beme", "logbeme", "epdummy", "eplus2p", "e2p", "fyear") :=
           list(as.integer(PERMNO), as.numeric(retdadjpct), as.numeric(ME), as.numeric(logme), as.numeric(juneme), as.numeric(junelogme), as.numeric(be),
                as.numeric(beme), as.numeric(logbeme), as.numeric(epdummy), as.numeric(eplus2p), as.numeric(e2p), as.integer(fyear))]

any(duplicated(dat.size[, c("DATE", "PERMNO", "fyear")]))

dat.size.split <- split(dat.size, dat.size$DATE)

chris.fit.classic.size <- lapply(dat.size.split, function(x) {
        x <- na.omit(x, c("retdadjpct", "junelogme"))
        unname(coef(lm.fit(x = cbind(1, as.matrix(x$junelogme)), y = x$retdadjpct)))
})

chris.fit.classic.book_market <- lapply(dat.size.split, function(x) {
        x <- na.omit(x, c("retdadjpct", "logbeme"))
        unname(coef(lm.fit(x = cbind(1, as.matrix(x$logbeme)), y = x$retdadjpct)))
})

chris.fit.classic.book_market.mat <- matrix(unlist(chris.fit.classic.book_market), ncol = 2, byrow = TRUE)

dates.chris <- as.Date(names(chris.fit.classic.book_market), "%Y%m%d")

chris.fit.classic.book_market.dt <- data.table(date = dates.chris, beta_0 = chris.fit.classic.book_market.mat[, 1], beta_1 = chris.fit.classic.book_market.mat[, 2])

chris.fit.classic.book_market.dt <- chris.fit.classic.book_market.dt[year(date) >= 1980]

chris.fit.classic.book_market.jan.dt <- chris.fit.classic.book_market.dt[month(date) == 1]

fit <- lm(beta_1 ~ 1, chris.fit.classic.book_market.dt)

chris.book_market.test <- coeftest(fit, vcov. = NeweyWest(fit, lag = 6, prewhite = FALSE))

ggplot() +
        geom_line(data = chris.fit.classic.book_market.dt, aes(x = date, y = beta_1), col = "blue") +
        geom_point(data = chris.fit.classic.book_market.jan.dt, aes(x = date, y = beta_1), size = 1) +
        ggtitle("Time Series of Book-to-Market Slopes") +
        xlab("Date") + 
        ylab("Slope") +
        theme_bw()

## Chris Earnings-to-Price

chris.fit.classic.earnings_price <- lapply(dat.size.split, function(x) {
        x <- na.omit(x, c("retdadjpct", "epdummy", "eplus2p"))
        unname(coef(lm.fit(x = cbind(1, as.matrix(x[, .(epdummy, eplus2p)])), y = x$retdadjpct)))
})

chris.fit.classic.earnings_price.dt <- data.table(matrix(unlist(chris.fit.classic.earnings_price), ncol = 3, byrow = TRUE))

colnames(chris.fit.classic.earnings_price.dt) <- c("beta_0", "beta_1", "beta_2")

chris.fit.classic.earnings_price.dt <- cbind(data.table(date = dates.chris), chris.fit.classic.earnings_price.dt)

chris.fit.classic.earnings_price.jan.dt <- chris.fit.classic.earnings_price.dt[month(date) == 1]

ggplot() +
        geom_line(data = chris.fit.classic.earnings_price.dt, aes(x = date, y = beta_1), col = "dodgerblue2") +
        geom_point(data = chris.fit.classic.earnings_price.jan.dt, aes(x = date, y = beta_1), size = 1) +
        ggtitle("Time Series of EP-dummy Slopes") +
        xlab("Date") + 
        ylab("Slope") +
        theme_bw()

fit <- lm(beta_1 ~ 1, chris.fit.classic.earnings_price.dt)

chris.earnings_price.test <- coeftest(fit, vcov. = NeweyWest(fit, lag = 6, prewhite = FALSE))

ggplot() +
        geom_line(data = chris.fit.classic.earnings_price.dt, aes(x = date, y = beta_2), col = "dodgerblue2") +
        geom_point(data = chris.fit.classic.earnings_price.jan.dt, aes(x = date, y = beta_2), size = 1) +
        ggtitle("Time Series of Positive Earnings-to-Price Slopes") +
        xlab("Date") + 
        ylab("Slope") +
        theme_bw()

fit <- lm(beta_2 ~ 1, chris.fit.classic.earnings_price.dt)

chris.earnings_price.test2 <- coeftest(fit, vcov. = NeweyWest(fit, lag = 6, prewhite = FALSE))

## Combined

book_market.dat <- fit.book_market.dt[year(date) >= 1980 & year(date) <= 2015]

book_market.dat[["LS_CHRIS"]] <- chris.fit.classic.book_market.dt[year(date) >= 1980]$beta_1

setcolorder(book_market.dat, c("date", "LS_CHRIS", "LS", "ROBUST"))

ggplot() +
        geom_line(data = book_market.melted, aes(x = date, y = value), col = "dodgerblue2") +
        geom_point(data = book_market.jan.melted, aes(x = date, y = value), size = 1) +
        facet_grid(rows = vars(variable), switch = "both") +
        geom_hline(yintercept = 0) +
        # geom_hline(yintercept = -3, data = subset(fit.size.dt.melted, variable == "LS")) +
        # geom_hline(yintercept = -4, data = subset(fit.size.dt.melted, variable == "ROBUST")) +
        ggtitle("Time Series of Book-to-Market Slopes") +
        xlab("Date") + 
        scale_y_continuous(position = "right", limits = c(-6, 6)) +
        theme_bw() +
        theme(axis.title.y = element_blank())

book_market.melted <- melt(book_market.dat, id.vars = "date")

book_market.jan.melted <- book_market.melted[month(date) == 1]