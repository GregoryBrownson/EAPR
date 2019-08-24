#' Fama MacBeth Cross-Sectional Regressions
#'
#' @export fama_macbeth
#' @rdname fama_macbeth
#'
#' @description Computes cross-sectional regressions for each period 
#'
#' @param formula An eapr object which contains the time series of variables for all
#' stocks.
#' @param x Either a vector of percentile cut points or the number of quantiles
#' to compute. If the latter, quantiles are computed using evenly spaced percentile
#' cut points.
#' @param type Type(s) of regression method(s) to use. Current supported methods are least-squares and robust MM regressions. 
#' Least-squares may be selected by giving this parameter a value 'ols' or 'ls' and Robust MM is chosen by giving this parameter a value of 'mm' or 
#' 'robust.mm'.
#' @param outliers.method Method for dealing with outliers. Options are 'trim', 'winzorize', and 'none'.
#' @param outliers.level Level to use for outliers. Indicates that whichever method chosen, unless 'none', will be applied to the x and 1 - x percentiles.
#' @param robust.control Specifies model parameters for robust regression. See \code{\link[RobStatTM]{lmrobdet.control}} for more info.
#' 
#' @return Data.table containing a time series of intercepts and slopes if only one regression method is selected. A list of data.tables if multiple
#' regression methods selected.
#' 
#' @author Gregory Brownson, \email{gregory.brownson@gmail.com}

fama_macbeth <- function(formula, x, type = "ols", outliers.method = "winsorize", outliers.level = 0.005, robust.control = NULL) {
  if (any(type != "ols") & is.null(robust.control)) {
    robust.control <- lmrobdet.control(efficiency = 0.99, family = "optimal")
  }
  
  if (outliers.method %in% c("winsorize", "trim", "none") == FALSE) {
    stop(paste0(outliers.method, " not a valid method!"))
  }
  
  type <- tolower(unique(type))
  
  type <- gsub("ols", "ls", type)
  type <- gsub("^mm", "robust.mm", type)
  
  cols <- c("date", "permno", all.vars(formula))
  
  dat <- x$ccm[, ..cols]
  
  dat.split <- split(dat, dat$date)
  
  dependent <- all.vars(formula)[-1]
  
  if (outliers.method == 'none') {
    if (length(type) == 1) {
      fun <- switch(type,
                    ls  = "lm",
                    robust.mm = "lmrobdetMM",
                    NULL)
      
      if (is.null(fun)) {
        stop(paste("Invalid model type:", type))
      }
      
      coefs <- lapply(dat.split, function(dt, fun, fm) {
                        x <- na.omit(dt)
                        coef(do.call(fun, list(formula = fm, data = x)))
                      },
                      fun = fun,
                      fm = formula)
      
      dates <- as.Date(names(coefs))
      
      dat.list <- list()
      
      dat.list[[fun]] <- cbind(data.table::data.table(date = dates), data.table::data.table(Reduce(rbind, coefs)))
    } else {
      dat.list <- lapply(type, function(model, dt.split, formula, robust.control) {
                           fun <- switch(model,
                                        ols = "lm",
                                        ls  = "lm",
                                        mm  = "lmrobdetMM",
                                        robust = "lmrobdetMM",
                                        NULL)
                           
                           if (is.null(fun)) {
                             stop(paste("Invalid model type:", model))
                           }
                           
                           if (fun == "lm") {
                             coefs <- lapply(dt.split, function(dt, fun, fm) {
                                               x <- na.omit(dt)
                                               coef(lm(formula, data = x))
                                             },
                                             fun = fun,
                                             fm  = formula)
                           } else {
                             coefs <- lapply(dt.split, function(dt, fun, fm, control) {
                                               x <- na.omit(dt)
                                               coef(do.call(fun, list(formula = fm, data = x, control = control)))
                                             },
                                             fun = fun,
                                             fm = formula,
                                             control = robust.control)
                           }
                           dates <- as.Date(names(coefs))
                           
                           cbind(data.table::data.table(date = dates), data.table::data.table(Reduce(rbind, coefs)))
                         },
                         dt.split  = dat.split,
                         formula   = formula,
                         robust.control = robust.control)
    }
  } else {
    if (length(type) == 1) {
      fun <- switch(type,
                    ls  = "lm",
                    robust.mm = "lmrobdetMM",
                    NULL)
      
      if (is.null(fun)) {
        stop(paste("Invalid model type:", type))
      }
      
      if (fun == "lm") {
        coefs <- lapply(dat.split, function(dt, fun, fm, dependent, ot, ot.lev) {
                          x <- na.omit(dt)
                          x <- do.call(ot, list(x = x, level = ot.lev, vars = dependent))
                          coef(do.call(fun, list(formula = fm, data = x)))
                        },
                        fun = fun,
                        fm = formula,
                        dependent = dependent,
                        ot  = outliers.method,
                        ot.lev = outliers.level)
      } else {
        coefs <- lapply(dat.split, function(dt, fun, fm, dependent, control) {
                          x <- na.omit(dt)
                          coef(do.call(fun, list(formula = fm, data = x, control = control)))
                        },
                        fun = fun,
                        fm = formula,
                        dependent = dependent,
                        control = robust.control)
      }
      
      dates <- as.Date(names(coefs))
      
      dat.list <- list()
      
      dat.list[[fun]] <- cbind(data.table::data.table(date = dates), data.table::data.table(Reduce(rbind, coefs)))
    } else {
      dat.list <- lapply(type, function(model, dt.split, formula, dependent, outliers.method, outliers.level, robust.control) {
                           fun <- switch(model,
                                         ols = "lm",
                                         ls  = "lm",
                                         mm  = "lmrobdetMM",
                                         robust = "lmrobdetMM",
                                         NULL)
                           
                           if (is.null(fun)) {
                             stop(paste("Invalid model type:", model))
                           }
                          
                           if (fun == "lm") {
                             coefs <- lapply(dt.split, function(dt, fm, dependent, ot, ot.lev) {
                                               x <- na.omit(dt[, ..cols])
                                               x <- do.call(ot, list(x = x, level = ot.lev, vars = dependent))
                                               coef(lm(formula, data = x))
                                             },
                                             fm = formula,
                                             dependent = dependent,
                                             ot  = outliers.method,
                                             ot.lev = outliers.level)
                           } else {
                             coefs <- lapply(dt.split, function(dt, fun, fm, dependent, control) {
                                               x <- na.omit(dt)
                                               coef(do.call(fun, list(formula = fm, data = x, control = control)))
                                             },
                                             fun = fun,
                                             fm = formula,
                                             dependent = dependent,
                                             control = robust.control)
                           }
                           dates <- as.Date(names(coefs))
                          
                           cbind(data.table::data.table(date = dates), data.table::data.table(Reduce(rbind, coefs)))
                         },
                         dt.split = dat.split,
                         formula = formula,
                         dependent = dependent,
                         outliers.method  = outliers.method,
                         outliers.level = outliers.level,
                         robust.control = robust.control)
    }
  }
  
  names(dat.list) <-  type
  
  class(dat.list) <- "fama_macbeth"
  
  return(dat.list)
}

winsorize <- function(x, level, vars) {
  for (i in 1:length(vars)) {
    q <- quantile(x[[vars[i]]], c(level, 1 - level))
    x[[vars[i]]] <- ifelse(x[[vars[i]]] < q[1], q[1], x[[vars[i]]])
    x[[vars[i]]] <- ifelse(x[[vars[i]]] > q[2], q[2], x[[vars[i]]])
  }

  return(x)
}

trim <- function(x, vars, level) {
  for (i in 1:length(vars)) {
    q <- quantile(x[[vars[i]]], c(level, 1 - level))
    x <- x[x[[vars[i]]] > q[1] & x[[vars[i]]] < q[2]]
  }

  return(x)
}

summary.fama_macbeth <- function(x) {
  
}

plot.fama_macbeth <- function(x) {
  
}