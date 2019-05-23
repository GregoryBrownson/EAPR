library(data.table)
library(devtools)
library(ggplot2)
library(purrr)
library(RobStatTM)
library(tidyr)
library(xts)


devtools::load_all(".")

eapr <- EAPR::extract("gsb25")

data.split <- split(eapr$ccm, eapr$ccm$date)

fit.classic.size <- lapply(data.split, function(x) {
                             x <- na.omit(x, c("adj_ret", "log_market_equity"))
                             unname(coef(lm.fit(x = cbind(1, as.matrix(x$log_market_equity)), y = x$adj_ret)))[2]
                           })

fit.robust.size <- lapply(data.split, function(x) {
                            x <- na.omit(x, c("adj_ret", "log_market_equity"))
                            control <- lmrobdet.control(efficiency = 0.99)
                            lmrobdetMM(adj_ret ~ log_market_equity, data = x, control = control)
                          })



dates <- names(unlist(fit.classic.size))

fit.classic.size.dt <- data.table(date = as.Date(dates), beta = unlist(fit.classic.size))

fit.classic.size.dt <- fit.classic.size.dt[date >= as.Date("1963-07-01") & year(date) <= 1990]

fit.classic.size.jan.dt <- fit.classic.size.dt[month(date) == 1 & year(date) >= 1964 & year(date) <= 1990]

ggplot() +
  geom_line(data = fit.classic.size.dt, aes(x = date, y = beta), col = "blue") +
  geom_point(data = fit.classic.size.jan.dt, aes(x = date, y = beta), col = "black")
