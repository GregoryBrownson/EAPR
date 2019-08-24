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
#' @param outliers Method for dealing with outliers. Options are 'trim', 'winzorize', and 'none'.
#' @param outliers.level Level to use for outliers. Indicates that whichever method chosen, unless 'none', will be applied to the x and 1 - x percentiles.
#' @param robust.control Specifies model parameters for robust regression. See \code{\link[RobStatTM]{lmrobdet.control}} for more info.
#' 
#' @return Data.table containing a time series of intercepts and slopes if only one regression method is selected. A list of data.tables if multiple
#' regression methods selected.
#' 
#' @author Gregory Brownson, \email{gregory.brownson@gmail.com}

fama_macbeth <- function(formula, x, type = "ols", outliers = "winsorize", outliers.level = 0.005, robust.control = NULL) {
  if (any(type != "ols") & is.null(robust.control)) {
    robust.control <- lmrobdet.control(efficiency = 0.99, family = "optimal")
  }
  
  if (outliers %in% c("winsorize", "trim", "none") == FALSE) {
    stop(paste0(outliers, " not a valid method!"))
  }
  
  type <- tolower(unique(type))
  
  type <- gsub("ols", "ls", type)
  type <- gsub("^mm", "robust.mm", type)
  
  dat <- model.frame(formula, data = x$ccm)
  
  dat.split <- split(dat.filtered$ccm, dat.filtered$ccm$date)
  
  dependent <- colnames(dat)[2:ncol(dat)]
  
  if (outliers == 'none') {
    if (length(type) == 1) {
      fun <- switch(type,
                    ls  = "lm",
                    robust.mm = "lmrobdetMM",
                    NULL)
      
      if (is.null(fun)) {
        stop(paste("Invalid model type:", type))
      }
      
      coefs <- lapply(dat.split, function(dt, fm, fun, dependent) {
               dt <- na.omit(x)
               coef(do.call(fun, list(formula = fm, data = dt)))
             },
             fm = formula,
             fun = fun,
             dependent = dependent)
      
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
                               dt <- na.omit(dt)
                               coef(lm(formula, data = dt))
                             },
                             fun = fun,
                             fm  = formula,
                             dependent = dependent)
                           } else {
                             coefs <- lapply(dt.split, function(dt, fun, fm) {
                               dt <- na.omit(dt)
                               coef(do.call(fun, list(formula = fm, data = dt)))
                             },
                             fun = fun,
                             fm = formula,
                             dependent = dependent)
                           }
                           dates <- as.Date(names(fit.classic.size))
                           
                           cbind(data.table::data.table(date = dates), data.table::data.table(Reduce(rbind, coefs)))
                         },
                         dt.split  = dat.split,
                         formula   = formula,
                         dependent = dependent,
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
      
      coefs <- lapply(dat.split, function(dt, fm, fun, ot, ot.lev) {
        x <- na.omit(x, c("adj_ret", "log_rDate_market_equity"))
        coef(do.call(fun, list(fm, dt)))
      },
      fm = formula,
      fun = fun,
      ot  = outliers,
      ot.lvl = outliers.level)
      
      dates <- as.Date(names(coefs))
      
      dat.list <- list()
      
      dat.list[[fun]] <- cbind(data.table::data.table(date = dates), data.table::data.table(Reduce(rbind, coefs)))
    } else {
      dat.list <- lapply(type, function(model, dt.split, formula, control) {
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
          coefs <- lapply(dt.split, function(dt, fun, fm, ot, ot.lev) {
            dependent <- 
            dt <- na.omit(dt, c("adj_ret", "log_rDate_market_equity"))
            coef(lm(formula, data = x))
          },
          fun = fun,
          fm = formula,
          ot  = outliers,
          ot.lev = outliers.level)
        } else {
          coefs <- lapply(dt.split, function(dt, fun, fm, ot, ot.lev, control) {
            dt <- na.omit(dt, c("adj_ret", "log_rDate_market_equity"))
            coef(do.call(fun, list(formula = fm, data = dt, control = control)))
          },
          fun = fun,
          fm = formula,
          ot  = outliers,
          ot.lev = outliers.level,
          control = robust.control)
        }
        dates <- as.Date(names(fit.classic.size))
        
        cbind(data.table::data.table(date = dates), data.table::data.table(Reduce(rbind, coefs)))
      },
      dt.split = dat.split,
      formula = formula,
      ot  = outliers,
      ot.lvl = outliers.level,
      robust.control = robust.control)
    }
  }
  
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
    x <- x[get(vars[i]) > q[1] & get(vars[i]) < q[2]]
  }
  
  return(x)
}

summary.fama_macbeth <- function(x) {
  
}

plot.fama_macbeth <- function(x) {
  
}