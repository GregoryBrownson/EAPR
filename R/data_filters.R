#' Implementation of filter outlined in Fama and French (1992).
#' @export
filter.ff <- function(crsp) {

  crsp <- crsp %>%
    group_by(permco, period) %>%
    mutate(month = month(date),
           has_Jun = any(month == 6),
           has_Dec = any(month == 12))

  crsp <- filter(crsp, has_Jun == TRUE & has_Dec == TRUE)

  drop <- c("has_Jun", "has_Dec")

  crsp[, c(drop) := NULL]

  # comp <- comp %>%
  #   group_by(permno, date) %>%
  #   mutate(is_valid = !(is.na(book_equity) || is.na(assets) || is.na(earnings)))
  #
  # comp <- filter(comp, is_valid == TRUE)
  #
  # comp[, is_valid := NULL]

  return(crsp)
}

