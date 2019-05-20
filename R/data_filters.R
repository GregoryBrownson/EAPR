#' Implementation of filter outlined in Fama and French (1992).
#'
#' @importFrom data.table ':='
#' @importFrom dplyr group_by mutate filter '%>%'
#'
#'
#' @export
#'
#'
filter <- function(x, type) {
  for (i in 1:length(type)) {
    x <- do.call(paste("filter", type[i], sep = '.'), x)
  }
  
  return(x)
}

#' 
filter.ff <- function(x) {
  x <- x %>%
    group_by(permco, period) %>%
    mutate(month = month(date),
           has_Jun = any(month == 6),
           has_Dec = any(month == 12))

  x <- filter(x, has_Jun == TRUE & has_Dec == TRUE & is_financial == FALSE)

  drop <- c("has_Jun", "has_Dec")

  x[, c(drop) := NULL]

  # comp <- comp %>%
  #   group_by(permno, date) %>%
  #   mutate(is_valid = !(is.na(book_equity) || is.na(assets) || is.na(earnings)))
  #
  # comp <- filter(comp, is_valid == TRUE)
  #
  # comp[, is_valid := NULL]

  return(x)
}

