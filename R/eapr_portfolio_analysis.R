# Calculates post-ranking beta for returns on sorted portfolios, as described in Fama and French (1992)

summary.portfolio.eapr <- function(x)

mean.portfolio.eapr <-
  
stdev.portfolio.eapr <-
  
variance.portfolio.eapr <-
  
postRankBeta <- function(x, preceding, min.prec, type = "classic") {
  stopifnot(all(type %in% c('classic', 'robust')))
  
  min <- as.integer(preceding * min.prec)
  
  if (nrow(x) < min) {
    return(data.table(date = x$date, post_rank_beta = NA))
  }
  
  start_date <- max(ceiling_date(x$date[1] %m-% months(6), "year") %m+% months(6) - days(1), as.Date("1963-06-30"))
  
  indx <- seq(max(min, as.integer(sum(x$date <= start_date))), nrow(x), by = 1)
  
  betas <- lapply(indx, function (i, data, type) { computeDimsonBeta(data[1:i], type) },
                  data = x,
                  type = type)
  
  names <- paste0("post_rank_beta_", names(betas[[1]]))
  
  betas <- matrix(unlist(betas), ncol = length(type), byrow = TRUE)
  
  colnames(betas) <- names
  
  betas <- data.table(x[indx, "date"], betas)
  
  return(betas)
}