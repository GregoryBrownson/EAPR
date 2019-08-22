#rm(list = ls())

library(data.table)
library(devtools)
library(ggplot2)
library(gridExtra)
library(lmtest)
library(purrr)
library(RobStatTM)
library(sandwich)
library(tables)
library(xts)

devtools::load_all(".")

eapr <- EAPR::extract("gsb25")

dat.filtered <- EAPR::filter.ff92(eapr)

data.split <- split(dat.filtered$ccm, dat.filtered$ccm$date)

## SIZE

# LS and Robust regressions
fit.classic.size <- lapply(data.split, function(x) {
  x <- na.omit(x, c("adj_ret", "log_rDate_market_equity"))
  unname(coef(lm(adj_ret * 100 ~ log_rDate_market_equity, data = x)))
})

fit.robust.size <- lapply(data.split, function(x) {
  x <- na.omit(x, c("adj_ret", "log_rDate_market_equity"))
  control <- lmrobdet.control(efficiency = 0.99)
  unname(coef(lmrobdetMM(adj_ret * 100 ~ log_rDate_market_equity, data = x, control = control)))
})

dates <- as.Date(names(fit.classic.size))

fit.classic.size.dt <- data.table(matrix(unlist(fit.classic.size), ncol = 2, byrow = TRUE))

colnames(fit.classic.size.dt) <- c("beta_0", "beta_1")

fit.classic.size.dt <- cbind(data.table(date = dates), fit.classic.size.dt)

fit.robust.size.dt <- data.table(matrix(unlist(fit.robust.size), ncol = 2, byrow = TRUE))

colnames(fit.robust.size.dt) <- c("beta_0", "beta_1")

fit.robust.size.dt <- cbind(data.table(date = dates), fit.robust.size.dt)

fit.size.dt <- data.table(date = dates, LS = fit.classic.size.dt$beta_1, ROBUST = fit.robust.size.dt$beta_1)

fit.size.dt.melted <- melt(fit.size.dt, id.vars = "date")

fit.size.jan.dt.melted <- fit.size.dt.melted[month(date) == 1]

y.lim <- ceiling(max(abs(fit.classic.size.dt$beta_1)))

jpeg("./inst/etc/img/LS_size_slopes_1963_2018.jpeg", width = 700, height = 500)
ggplot() +
  geom_line(data = fit.classic.size.dt, aes(x = date, y = beta_1), col = "dodgerblue2") +
  geom_point(data = fit.classic.size.dt[month(date) == 1], aes(x = date, y = beta_1), size = 1) +
  geom_hline(yintercept = 0) +
  ggtitle("Time Series of LS Size Slopes 1963-2018") +
  xlab("Date") + 
  ylab("Slope") +
  scale_y_continuous(limits = c(-y.lim, y.lim)) +
  scale_x_date(date_breaks = "10 years", date_minor_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(axis.title.y = element_blank())
dev.off()

y.lim <- ceiling(max(abs(fit.size.dt.melted[year(date) <= 1990]$value)))

jpeg("./inst/etc/img/size_slopes_1963_1990.jpeg", width = 700, height = 500)
ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = fit.size.dt.melted[year(date) <= 1990], aes(x = date, y = value), col = "dodgerblue2") +
  geom_point(data = fit.size.jan.dt.melted[year(date) <= 1990], aes(x = date, y = value), size = 1) +
  facet_grid(rows = vars(variable), switch = "both") +
  ggtitle("Time Series of Size Slopes 1963-1990") +
  xlab("Date") +
  scale_y_continuous(position = "right", limits = c(-y.lim, y.lim)) +
  scale_x_date(date_breaks = "5 years", date_minor_breaks = "1 year", date_labels = "%Y") +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  theme(strip.background = element_rect(fill="orange2"))
dev.off()

y.lim <- ceiling(max(abs(fit.size.dt.melted[date > as.Date("1980-07-01")]$value)))

jpeg("./inst/etc/img/size_slopes_1980_2018.jpeg", width = 700, height = 500)
ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = fit.size.dt.melted[date > as.Date("1980-7-1")], aes(x = date, y = value), col = "dodgerblue2") +
  geom_point(data = fit.size.jan.dt.melted[date > as.Date("1980-7-1")], aes(x = date, y = value), size = 1) +
  facet_grid(rows = vars(variable), switch = "both") +
  ggtitle("Time Series of Size Slopes 1980-2018") +
  xlab("Date") +
  scale_y_continuous(position = "right", limits = c(-y.lim, y.lim)) +
  scale_x_date(date_breaks = "5 years", date_minor_breaks = "1 year", date_labels = "%Y") +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  theme(strip.background = element_rect(fill="orange2"))
dev.off()

# fit <- lm(beta_1 ~ 1, fit.classic.size.dt[year(date) <= 1990])
# 
# size.test <- coeftest(fit, vcov. = NeweyWest(fit, lag = 1, prewhite = FALSE))

standard_error.classic <- sqrt(lrvar(fit.classic.size.dt[year(date) <= 1990]$beta_1, type = "Newey-West"))

mu.size.classic <- mean(fit.classic.size.dt[year(date) <= 1990]$beta_1)

t_stat.size.classic <- mu.size.classic / standard_error.classic

standard_error.robust <- sqrt(lrvar(fit.robust.size.dt[year(date) <= 1990]$beta_1, type = "Newey-West"))

mu.size.robust <- mean(fit.robust.size.dt[year(date) <= 1990]$beta_1)

t_stat.size.robust <- mu.size.robust / standard_error.robust

size.report.dt <- data.table(Factor = c("", "Size", ""), Method = c("LS (FF92)", "LS (MB)", "Robust (MB)"),
                             "Slope (t-stat)" = c("-0.15 (-2.58)",
                                                  paste0(round(mu.size.classic, 2), " (", round(t_stat.size.classic, 2), ")"),
                                                  paste0(round(mu.size.robust, 2), " (", round(t_stat.size.robust, 2), ")")))

jpeg("./inst/etc/img/size_table_ff_comparison.jpeg", width = 700, height = 500)
plot.new()
grid.table(size.report.dt)
dev.off()

## BOOK-TO-MARKET

fit.classic.book_market <- lapply(data.split, function(x) {
  x <- na.omit(x, c("adj_ret", "log_book_market"))
  q <- quantile(x$log_book_market, c(0.005, 0.995))
  # x$log_book_market <- ifelse(x$log_book_market < q[1], q[1], x$log_book_market)
  # x$log_book_market <- ifelse(x$log_book_market > q[2], q[2], x$log_book_market)
  x <- x[log_book_market > q[1] & log_book_market < q[2]]
  unname(coef(lm(adj_ret * 100 ~ log_book_market, data = x)))
})

fit.robust.book_market <- lapply(data.split, function(x) {
  x <- na.omit(x, c("adj_ret", "log_book_market"))
  control <- lmrobdet.control(efficiency = 0.99)
  unname(coef(lmrobdetMM(adj_ret * 100 ~ log_book_market, data = x, control = control)))
})

dates <- as.Date(names(fit.classic.book_market))

fit.classic.book_market.dt <- data.table(matrix(unlist(fit.classic.book_market), ncol = 2, byrow = TRUE))

colnames(fit.classic.book_market.dt) <- c("beta_0", "beta_1")

fit.classic.book_market.dt <- cbind(data.table(date = dates), fit.classic.book_market.dt)

fit.robust.book_market.dt <- data.table(matrix(unlist(fit.robust.book_market), ncol = 2, byrow = TRUE))

colnames(fit.robust.book_market.dt) <- c("beta_0", "beta_1")

fit.robust.book_market.dt <- cbind(data.table(date = dates), fit.robust.book_market.dt)

fit.book_market.dt <- data.table(date = dates, LS = fit.classic.book_market.dt$beta_1, ROBUST = fit.robust.book_market.dt$beta_1)

fit.book_market.dt.melted <- melt(fit.book_market.dt, id.vars = "date")

fit.book_market.jan.dt.melted <- fit.book_market.dt.melted[month(date) == 1]

y.lim <- ceiling(max(abs(fit.classic.book_market.dt$beta_1)))

jpeg("./inst/etc/img/LS_book_market_slopes_1963_2018.jpeg", width = 700, height = 500)
ggplot() +
  geom_line(data = fit.classic.book_market.dt, aes(x = date, y = beta_1), col = "dodgerblue2") +
  geom_point(data = fit.classic.book_market.dt[month(date) == 1], aes(x = date, y = beta_1), size = 1) +
  geom_hline(yintercept = 0) +
  ggtitle("Time Series of LS Book-to-Market Slopes 1963-2018") +
  xlab("Date") + 
  ylab("Slope") +
  scale_y_continuous(limits = c(-y.lim, y.lim)) +
  scale_x_date(date_breaks = "10 years", date_minor_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(axis.title.y = element_blank())
dev.off()

y.lim <- ceiling(max(abs(fit.book_market.dt.melted[year(date) <= 1990]$value)))

jpeg("./inst/etc/img/book_market_slopes_1963_1990.jpeg", width = 700, height = 500)
ggplot() +
  geom_line(data = fit.book_market.dt.melted[year(date) <= 1990], aes(x = date, y = value), col = "dodgerblue2") +
  geom_point(data = fit.book_market.jan.dt.melted[year(date) <= 1990], aes(x = date, y = value), size = 1) +
  facet_grid(rows = vars(variable), switch = "both") +
  geom_hline(yintercept = 0) +
  ggtitle("Time Series of Book-to-Market Slopes 1963-1990") +
  xlab("Date") + 
  ylab("Slope") +
  scale_y_continuous(position = "right", limits = c(-y.lim, y.lim)) +
  scale_x_date(date_breaks = "5 years", date_minor_breaks = "1 year", date_labels = "%Y") +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  theme(strip.background = element_rect(fill="orange2"))
dev.off()

y.lim <- ceiling(max(abs(fit.book_market.dt.melted[date > as.Date("1980-7-1")]$value)))

jpeg("./inst/etc/img/book_market_slopes_1980_2018.jpeg", width = 700, height = 500)
ggplot() +
  geom_line(data = fit.book_market.dt.melted[date > as.Date("1980-7-1")], aes(x = date, y = value), col = "dodgerblue2") +
  geom_point(data = fit.book_market.jan.dt.melted[date > as.Date("1980-7-1")], aes(x = date, y = value), size = 1) +
  facet_grid(rows = vars(variable), switch = "both") +
  geom_hline(yintercept = 0) +
  ggtitle("Time Series of Book-to-Market Slopes 1980-2018") +
  xlab("Date") + 
  ylab("Slope") +
  scale_y_continuous(position = "right", limits = c(-y.lim, y.lim)) +
  scale_x_date(date_breaks = "5 years", date_minor_breaks = "1 year", date_labels = "%Y") +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  theme(strip.background = element_rect(fill="orange2"))
dev.off()

# fit <- lm(beta ~ 1, fit.classic.book_market.dt)
# 
# book_market.test <- coeftest(fit, vcov. = NeweyWest(fit, lag = 6, prewhite = FALSE))

standard_error.classic <- sqrt(lrvar(fit.classic.book_market.dt[year(date) <= 1990]$beta_1, type = "Newey-West"))

mu.book_market.classic <- mean(fit.classic.book_market.dt[year(date) <= 1990]$beta_1)

t_stat.book_market.classic <- mu.book_market.classic / standard_error.classic

standard_error.robust <- sqrt(lrvar(fit.robust.book_market.dt[year(date) <= 1990]$beta_1, type = "Newey-West"))

mu.book_market.robust <- mean(fit.robust.book_market.dt[year(date) <= 1990]$beta_1)

t_stat.book_market.robust <- mu.book_market.robust / standard_error.robust

book_market.report.dt <- data.table(Factor = c("", "ln(BE/ME)", ""), Method = c("LS (FF92)", "LS (MB)", "Robust (MB)"),
                             "Slope (t-stat)" = c("0.50 (5.71)",
                                                  paste0(round(mu.book_market.classic, 2), " (", round(t_stat.book_market.classic, 2), ")"),
                                                  paste0(round(mu.book_market.robust, 2), " (", round(t_stat.book_market.robust, 2), ")")))

jpeg("./inst/etc/img/book_market_table_ff_comparison.jpeg", width = 700, height = 500)
plot.new()
grid.table(book_market.report.dt)
dev.off()

## Earnings-to-Price

fit.classic.earnings_price <- lapply(data.split, function(x) {
  x <- na.omit(x, c("adj_ret", "earnings_price"))
  q <- quantile(x$earnings_price, c(0.005, 0.995))
  x$earnings_price <- ifelse(x$earnings_price < q[1], q[1], x$earnings_price)
  x$earnings_price <- ifelse(x$earnings_price > q[2], q[2], x$earnings_price)
  # x <- x[earnings_price > q[1] & earnings_price < q[2]]
  unname(coef(lm(adj_ret * 100 ~ earnings_price, data = x)))
})

fit.classic.earnings_price2 <- lapply(data.split, function(x) {
  x <- na.omit(x, c("adj_ret", "earnings_price"))
  q <- quantile(x$earnings_price, c(0.005, 0.995))
  # x$earnings_price <- ifelse(x$earnings_price < q[1], q[1], x$earnings_price)
  # x$earnings_price <- ifelse(x$earnings_price > q[2], q[2], x$earnings_price)
  x <- x[earnings_price > q[1] & earnings_price < q[2]]
  unname(coef(lm(adj_ret * 100 ~ earnings_price, data = x)))
})

fit.classic.earnings_price3 <- lapply(data.split, function(x) {
  x <- na.omit(x, c("adj_ret", "earnings_price"))
  # q <- quantile(x$earnings_price, c(0.005, 0.995))
  # x$earnings_price <- ifelse(x$earnings_price < q[1], q[1], x$earnings_price)
  # x$earnings_price <- ifelse(x$earnings_price > q[2], q[2], x$earnings_price)
  # x <- x[earnings_price > q[1] & earnings_price < q[2]]
  unname(coef(lm(adj_ret * 100 ~ earnings_price, data = x)))
})

fit.robust.earnings_price <- lapply(1:length(data.split), function(i, dt) {
  x <- na.omit(data.split[[i]], c("adj_ret", "earnings_price"))
  control <- lmrobdet.control(efficiency = 0.99)
  
  if (i %in% c(16, 21, 460)) {
    control$family <- "modopt"
  }
  
  unname(coef(lmrobdetMM(adj_ret * 100 ~ earnings_price, data = x, control = control)))
},
dt = data.split)

# options(warn=2)
# 16 (1964-10-31), 21 (1965-03-31), 460 (2001-10-31)

dates <- as.Date(names(fit.classic.earnings_price))

# dates[c(16, 21, 460)] "1964-10-31" "1965-03-31" "2001-10-31" <- robust MM not converging on these dates

fit.classic.earnings_price.dt <- data.table(matrix(unlist(fit.classic.earnings_price), ncol = 2, byrow = TRUE))

colnames(fit.classic.earnings_price.dt) <- c("beta_0", "beta_1")

fit.classic.earnings_price.dt <- cbind(data.table(date = dates), fit.classic.earnings_price.dt)

fit.classic.earnings_price.dt2 <- data.table(matrix(unlist(fit.classic.earnings_price2), ncol = 2, byrow = TRUE))

colnames(fit.classic.earnings_price.dt2) <- c("beta_0", "beta_1")

fit.classic.earnings_price.dt2 <- cbind(data.table(date = dates), fit.classic.earnings_price.dt2)

fit.classic.earnings_price.dt3 <- data.table(matrix(unlist(fit.classic.earnings_price3), ncol = 2, byrow = TRUE))

colnames(fit.classic.earnings_price.dt3) <- c("beta_0", "beta_1")

fit.classic.earnings_price.dt3 <- cbind(data.table(date = dates), fit.classic.earnings_price.dt3)

fit.robust.earnings_price.dt <- data.table(matrix(unlist(fit.robust.earnings_price), ncol = 2, byrow = TRUE))

colnames(fit.robust.earnings_price.dt) <- c("beta_0", "beta_1")

fit.robust.earnings_price.dt <- cbind(data.table(date = dates), fit.robust.earnings_price.dt)

fit.earnings_price.dt <- data.table(date = dates, LS = fit.classic.earnings_price.dt3$beta_1,
                                    LS_WINSOR = fit.classic.earnings_price.dt$beta_1, LS_TRIMMED = fit.classic.earnings_price.dt2$beta_1)

fit.earnings_price.dt.melted <- melt(fit.earnings_price.dt, id.vars = "date")

fit.earnings_price.jan.dt.melted <- fit.earnings_price.dt.melted[month(date) == 1]

y.lim <- ceiling(max(abs(fit.classic.earnings_price.dt$beta_1)))

jpeg("./inst/etc/img/LS_earnings_price_slopes_1963_2018.jpeg", width = 700, height = 500)
ggplot() +
  geom_line(data = fit.classic.earnings_price.dt, aes(x = date, y = beta_1), col = "dodgerblue2") +
  geom_hline(yintercept = 0) +
  ggtitle("Time Series of Earnings-to-Price Slopes 1963-2018") +
  xlab("Date") +
  ylab("Slope") +
  scale_y_continuous(limits = c(-y.lim, y.lim)) +
  scale_x_date(date_breaks = "10 years", date_minor_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(axis.title.y = element_blank())
dev.off()

y.lim <- ceiling(max(abs(fit.earnings_price.dt.melted[year(date) <= 2015]$value)))

jpeg("./inst/etc/img/ep_chris_1963_2015.jpeg", width = 700, height = 500)
ggplot() +
  geom_line(data = fit.earnings_price.dt.melted[year(date) <= 2015], aes(x = date, y = value), col = "dodgerblue2") +
  facet_grid(rows = vars(variable), switch = "both") +
  geom_hline(yintercept = 0) +
  ggtitle("Time Series of Earnings-to-Price Slopes 1963-2015") +
  xlab("Date") +
  ylab("Slope") +
  scale_y_continuous(position = "right", limits = c(-y.lim, y.lim)) +
  scale_x_date(date_breaks = "10 years", date_minor_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  theme(strip.background = element_rect(fill="orange2"))
dev.off()

n_comp.lst <- lapply(data.split, nrow)

dates <- as.Date(ISOdate(year(as.Date(names(n_comp.lst)) %m-% months(6)), 7, 31))

n_comp <- data.table(Start_Date = dates, n = unname(unlist(n_comp.lst)))[year(dates) <= 2015]

n_comp <- n_comp %>%
            group_by(Start_Date) %>%
            summarise(My_min = min(n),
                      My_max = max(n)) %>%
            as.data.table

chris.n_comp <- fread("/home/gsb25/Desktop/EAPR_additional/data/chris_num_companies.csv")

chris.n_comp[, Start_Date := as.Date(Start_Date)]

n_comp.merged <- merge(chris.n_comp, n_comp, by = "Start_Date")

n_comp.melted <- melt(n_comp.merged, id.vars = "Start_Date")


png("./inst/etc/img/Num_companies.png", width = 800, height = 500)
ggplot() +
  geom_point(data = n_comp, mapping = aes(x = Start_Date, y = My_max, color = "Greg", shape = "Max"), size = 1.5) +
  geom_point(data = n_comp, mapping = aes(x = Start_Date, y = My_min, color = "Greg", shape = "Min"), size = 1.5) +
  geom_point(data = chris.n_comp, mapping = aes(x = Start_Date, y = Chris_max, color = "Chris", shape = "Max"), size = 1.5) + 
  geom_point(data = chris.n_comp, mapping = aes(x = Start_Date, y = Chris_min, color = "Chris", shape = "Min"), size = 1.5) +
  scale_color_manual(values = c("dodgerblue2", "red"))
dev.off()
## Leverage

# fit.classic.leverage <- lapply(data.split, function(x) {
#   x <- na.omit(x, c("adj_ret", "log_asset_market", "log_asset_book"))
#   
#   dat.winsor <- lapply(c("log_asset_market", "log_asset_book"), function(col, dt) {
#                          q <- quantile(dt[[col]], c(0.005, 0.995))
#                          temp <- ifelse(dt[[col]] < q[1], q[1], dt[[col]])
#                          ifelse(temp > q[2], q[2], temp)
#                        },
#                        dt = x)
#   
#   x$log_asset_market <- dat.winsor[[1]]
#   x$log_asset_book <- dat.winsor[[2]]
#   
#   unname(coef(lm(adj_ret * 100 ~ log_asset_market + log_asset_book, data = x)))
# })
# 
# fit.robust.leverage <- lapply(data.split, function(x) {
#   x <- na.omit(x, c("adj_ret", "log_rDate_market_equity"))
#   control <- lmrobdet.control(efficiency = 0.99)
#   unname(coef(lmrobdetMM(adj_ret * 100 ~ log_asset_market + log_asset_book, data = x, control = control)))
# })
# 
# dates <- as.Date(names(fit.classic.leverage))
# 
# fit.classic.leverage.dt <- data.table(matrix(unlist(fit.classic.leverage), ncol = 3, byrow = TRUE))
# 
# colnames(fit.classic.leverage.dt) <- c("beta_0", "beta_1", "beta_2")
# 
# fit.classic.leverage.dt <- cbind(data.table(date = dates), fit.classic.leverage.dt)
# 
# fit.robust.leverage.dt <- data.table(matrix(unlist(fit.robust.leverage), ncol = 3, byrow = TRUE))
# 
# colnames(fit.robust.leverage.dt) <- c("beta_0", "beta_1", "beta_2")
# 
# fit.robust.leverage.dt <- cbind(data.table(date = dates), fit.robust.leverage.dt)
# 
# # ASSET-TO-MARKET
# 
# fit.leverage.asset_market.dt <- data.table(date = dates, LS = fit.classic.leverage.dt$beta_1, ROBUST = fit.robust.leverage.dt$beta_1)
# 
# fit.leverage.asset_market.dt.melted <- melt(fit.leverage.asset_market.dt, id.vars = "date")
# 
# fit.leverage.asset_market.jan.dt.melted <- fit.leverage.asset_market.dt.melted[month(date) == 1]
# 
# y.lim <- ceiling(max(abs(fit.leverage.asset_market.dt.melted$value)))
# 
# ggplot() +
#   geom_hline(yintercept = 0) +
#   geom_line(data = fit.leverage.asset_market.dt.melted, aes(x = date, y = value), col = "dodgerblue2") +
#   geom_point(data = fit.leverage.asset_market.jan.dt.melted, aes(x = date, y = value), size = 1) +
#   facet_grid(rows = vars(variable), switch = "both") +
#   ggtitle("Time Series of Asset-to-Market Slopes 1963-1990") +
#   xlab("Date") +
#   scale_y_continuous(position = "right", limits = c(-y.lim, y.lim)) +
#   scale_x_date(date_breaks = "10 years", date_minor_breaks = "2 years", date_labels = "%Y") +
#   theme_bw() +
#   theme(axis.title.y = element_blank()) +
#   theme(strip.background = element_rect(fill="orange2"))
# 
# standard_error.classic <- sqrt(lrvar(fit.classic.leverage.dt[year(date) <= 1990]$beta_1, type = "Newey-West"))
# 
# mu.leverage.asset_market.classic <- mean(fit.classic.leverage.dt[year(date) <= 1990]$beta_1)
# 
# t_stat.leverage.asset_market.classic <- mu.leverage.asset_market.classic / standard_error.classic 
# 
# standard_error.robust <- sqrt(lrvar(fit.robust.leverage.dt[year(date) <= 1990]$beta_1, type = "Newey-West"))
# 
# mu.leverage.asset_market.robust <- mean(fit.robust.leverage.dt[year(date) <= 1990]$beta_1)
# 
# t_stat.leverage.asset_market.robust <- mu.leverage.asset_market.robust / standard_error.robust
# 
# leverage.asset_market.report.dt <- data.table(Factor = c("", "ln(A/ME)", ""), Method = c("LS (FF92)", "LS (MB)", "Robust (MB)"),
#                                     "Slope (t-stat)" = c("0.50 (5.69)",
#                                                          paste0(round(mu.leverage.asset_market.classic, 2), " (", round(t_stat.leverage.asset_market.classic, 2), ")"),
#                                                          paste0(round(mu.leverage.asset_market.robust, 2), " (", round(t_stat.leverage.asset_market.robust, 2), ")")))
# 
# plot.new()
# grid.table(leverage.asset_market.report.dt)
# 
# # ASSET-TO-BOOK
# 
# fit.leverage.asset_book.dt <- data.table(date = dates, LS = fit.classic.leverage.dt$beta_1, ROBUST = fit.robust.leverage.dt$beta_1)
# 
# fit.leverage.asset_book.dt.melted <- melt(fit.leverage.asset_book.dt, id.vars = "date")
# 
# fit.leverage.asset_book.jan.dt.melted <- fit.leverage.asset_book.dt.melted[month(date) == 1]
# 
# y.lim <- ceiling(max(abs(fit.leverage.asset_book.dt.melted$value)))
# 
# ggplot() +
#   geom_hline(yintercept = 0) +
#   geom_line(data = fit.leverage.asset_book.dt.melted, aes(x = date, y = value), col = "dodgerblue2") +
#   geom_point(data = fit.leverage.asset_book.jan.dt.melted, aes(x = date, y = value), size = 1) +
#   facet_grid(rows = vars(variable), switch = "both") +
#   ggtitle("Time Series of Asset-to-Book Slopes 1963-1990") +
#   xlab("Date") +
#   scale_y_continuous(position = "right", limits = c(-y.lim, y.lim)) +
#   scale_x_date(date_breaks = "10 years", date_minor_breaks = "2 years", date_labels = "%Y") +
#   theme_bw() +
#   theme(axis.title.y = element_blank()) +
#   theme(strip.background = element_rect(fill="orange2"))
# 
# standard_error.classic <- sqrt(lrvar(fit.classic.leverage.dt[year(date) <= 1990]$beta_2, type = "Newey-West"))
# 
# mu.leverage.asset_book.classic <- mean(fit.classic.leverage.dt[year(date) <= 1990]$beta_2)
# 
# t_stat.leverage.asset_book.classic <- mu.leverage.asset_book.classic / standard_error.classic
# 
# standard_error.robust <- sqrt(lrvar(fit.robust.leverage.dt[year(date) <= 1990]$beta_2, type = "Newey-West"))
# 
# mu.leverage.asset_book.robust <- mean(fit.robust.leverage.dt[year(date) <= 1990]$beta_2)
# 
# t_stat.leverage.asset_book.robust <- mu.leverage.asset_book.robust / standard_error.robust
# 
# leverage.asset_book.report.dt <- data.table(Factor = c("", "ln(A/BE)", ""), Method = c("LS (FF92)", "LS (MB)", "Robust (MB)"),
#                                     "Slope (t-stat)" = c("-0.57 (-5.34)",
#                                                          paste0(round(mu.leverage.asset_book.classic, 2), " (", round(t_stat.leverage.asset_book.classic, 2), ")"),
#                                                          paste0(round(mu.leverage.asset_book.robust, 2), " (", round(t_stat.leverage.asset_book.robust, 2), ")")))
# 
# plot.new()
# grid.table(leverage.asset_book.report.dt)