#' GAMLSS bootstrap method
#'
#' @description Creates a random generation function for the missing
#'   values with bootstrap sample from the fitted GAMLSS model for the
#'   completely observed data.
#'
#' @param incomplete.data Data frame with missings on one variable.
#' @param R Boolean matrix with the response indicator.
#' @param fit Random sample generator method.
#'
#' @return Returns a imputation sample generator.
ImpGamlssBootstrap <- function(incomplete.data, fit, R, ...) {
  ## Imputation using the bootstrap predictive distribution

  available.cases <- subset(incomplete.data, R)
  ## Fit initial gamlss model with completely observed data.
  master.predict <- fit(available.cases, available.cases, ...)

  tryCatch(
  {
    function(...) {
      bootstrap.sample <- available.cases
      repeat {
        ## Replace observed part of the available cases with a
        ## random sample of the same distribution
        bootstrap.sample[,1] <- master.predict()
        ## Create random sample generator with
        ## bootstrap.sample as completely observed set and the
        ## observed part of the variables with missings as
        ## predictors.
        bootstrap.predict <- fit(bootstrap.sample,
                                 subset(incomplete.data, !R), ...)
        break
      }
      bootstrap.predict(...)
    }
  }, error = function(e) {
    function(model) {
      do.call(rep,
              args=list(NA, nrow(subset(incomplete.data, !R))))}
  }
  )
}
