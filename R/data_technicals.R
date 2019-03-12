

preRankBeta(dat.grouped, ret.dt, market.dt, preceding, periodicity) <- function(x, type = "LS") {
  dat.grouped

  # Take a list of dt separated by permno,
  ret.data <- merge(size.dat.19019[, c("DATE", "retd")], ind.rets, by.x = "DATE", by.y = "data_date")

  ret.data <- size.dat.17523[, c("DATE", "r", "vwretdexc90", "vwretdexc90_lag")]

  ret.data.ts <- zoo(ret.data[, 2:ncol(ret.data)], order.by = ret.data$DATE)

  betas <- rollapply(dat.grouped, preceding = 60, FUN = function(z) {
                       fit = lm(r ~ vwretdexc90 + vwretdexc90_lag, data = as.data.frame(z))
                       sum(fit$coefficients[2:3])
                     },
                     by.column = FALSE,
                     by = 12,
                     align = "right")

  return(betas)
}

postRankBeta(x) <- function(x, type = "LS") {

}
