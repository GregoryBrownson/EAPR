
lm.eapr <- function(z, independent, dependent, type = "ols", int = T, robust.control = NULL) {
  if (any(type != "ols") & is.null(robust.control)) {
    robust.control <- lmrobdet.control(efficiency = 0.99, family = "modopt")
  }
  
  if (length(independent) > 1) {
    independent <- paste0(independent, collapse = " + ")
  }
  
  fm <- as.formula(paste0(dependent, " ~ ", independent)) # Formula for model
  
  dt.subset <- z$ccm[, c("rebalance_date", dependent, independent)]
  
  dt.subset.dates <- split(dt.subset, "rebalance_date")
  
  if (length(type) == 1) {
    fun <- switch(type,
                  ols = "lm",
                  mm  = "lmrobdetMM",
                  NULL)
    
    if (is.null(fun)) {
      stop(paste("Invalid model type:", type))
    }
    
    lapply(dt.subset.dates, function(data, dep) {
            fit <- do.call(fun, list(fm, dt.subset))
          })
    
  } else {
    lapply(type, function(model, data, dep, control) {
             fun <- switch(model,
                           ols = "lm",
                           mm  = "lmrobdetMM",
                           NULL)
            
             if (is.null(fun)) {
               stop(paste("Invalid model type:", model))
             }
           },
           data = dt.subset,
           dep = dependent,
           control = robust.control)
  }
  
}

summary.lm.eapr <- function(x) {
  
}

plot.lm.eapr <- function(x) {
  
}