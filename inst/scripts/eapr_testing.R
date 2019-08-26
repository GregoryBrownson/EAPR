library(data.table)
library(devtools)
library(ggplot2)
library(purrr)
library(RobStatTM)
library(xts)

devtools::load_all(".")

eapr <- EAPR::extract("gsb25")

data.split <- split(eapr$market.dt, eapr$market.dt$permno)

ccm.date <- split(eapr$ccm, eapr$ccm$date)

names(ccm.date)

fits <- lapply(ccm.date, function(dt) {
          lm(adj_ret ~ log_market_equity - 1, data = dt, na.action = na.omit)
        })

coefs <- unlist(lapply(fits, function(f) unname(coef(f)) ))
