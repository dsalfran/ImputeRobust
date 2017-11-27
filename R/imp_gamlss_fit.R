#' GAMLSS imputation fit
#'
#' @description
#' This function takes a data set to fit a gamlss model and
#' another to predict the expected parameters values. It returns a
#' function that will generate a vector of random observations for the
#' predicted parameters. The amount of random observations is the
#' number of units on the dataset used to get such predictions.
#'
#' @param data Completely observed data frame to be used to fit a
#'   gamlss model estimate.
#' @param new.data Data frame used to predict the parameter values for
#'   some given right side x-values on the gamlss model.
#' @param family Family to be used for the response variable on the
#'   GAMLSS estimation.
#' @param n.ind.par Number of individual parameters to be
#'   fitted. Currently it only allows one or two because of stability
#'   issues for more parameters.
#' @param gam.mod list with the parameters of the GAMLSS imputation
#'   model.
#' @param mod.planb list with the parameters of the alternative GAMLSS
#'   imputation model.
#' @param n.par.planb number of individual parameters in the
#'   alternative model.
#' @param lin.terms Character vector specifying which (if any)
#'   predictor variables should enter the model linearly.
#' @param n.cyc number of cycles of the gamlss algorithm
#' @param bf.cyc number of cycles in the backfitting algorithm
#' @param cyc number of cycles of the fitting algorithm
#' @param forceNormal Flag that if set to 'TRUE' will use a normal
#'   family for the gamlss estimation as a last resource.
#' @param trace whether to print at each iteration (TRUE) or not
#'   (FALSE)
#' @param ... extra arguments for the control of the gamlss fitting
#'   function
#'
#' @return Returns a method to generate random samples for the fitted
#'   gamlss model using "new.data" as covariates.
ImpGamlssFit <- function(data, new.data, family, n.ind.par, gam.mod,
                         mod.planb = list(type = "pb", par = list(degree = 1, order = 1)),
                         n.par.planb = n.ind.par, lin.terms = NULL,
                         n.cyc = 5, bf.cyc = 5, cyc = 5,
                         forceNormal = FALSE, trace = FALSE, ...) {


  # Really ugly way of specifying the formulas for the gamlss fitting
  # function
  mu.f1 <- ModelCreator(data, gam.mod, lin.terms)
  sigma.f1 <- switch(n.ind.par, ~1, mu.f1, mu.f1, mu.f1)
  nu.f1 <- switch(n.ind.par, ~1, ~1, mu.f1, mu.f1)
  tau.f1 <- switch(n.ind.par, ~1, ~1, ~1, mu.f1)

  #Plan B
  mu.planb <- ModelCreator(data, mod.planb, lin.terms)
  sigma.planb <- switch(n.par.planb, ~1, mu.planb, mu.planb, mu.planb)
  nu.planb <- switch(n.par.planb, ~1, ~1, mu.planb, mu.planb)
  tau.planb <- switch(n.par.planb, ~1, ~1, ~1, mu.planb)
  if (forceNormal) {family.planb <- NO} else {family.planb <- family}

  tryCatch({
    # Fit gamlss model with given formula for all four moments
    # (ignored if not needed) and the given distribution family
    fit <- tryCatch(
      gamlss(formula = mu.f1,
             sigma.formula = sigma.f1,
             nu.formula = nu.f1,
             tau.formula = tau.f1,
             family = family,
             data = data,
             control = gamlss.control(n.cyc = n.cyc, trace = trace, ...),
             i.control = glim.control(bf.cyc = bf.cyc, cyc = cyc, ...)),
      error = function(e) {
        tryCatch(
          ## cat("PlanB\n")
          gamlss(formula = mu.f1,
                 sigma.formula = sigma.f1,
                 nu.formula = nu.f1,
                 tau.formula = tau.f1,
                 family = family,
                 data = data,
                 control = gamlss.control(n.cyc = min(3, n.cyc), trace = trace , ...),
                 i.control = glim.control(bf.cyc = min(4, bf.cyc), cyc = min(3, cyc), ...)),
          error = function(e) {
            tryCatch(
              ## cat("PlanC\n")
              gamlss(formula = mu.f1,
                     sigma.formula = sigma.f1,
                     nu.formula = nu.f1,
                     tau.formula = tau.f1,
                     family = family,
                     data = data,
                     control = gamlss.control(n.cyc = min(3, n.cyc), trace = trace , ...),
                     i.control = glim.control(bf.cyc = min(3, bf.cyc), cyc = min(2, cyc), ...)),
              error = function(e) {
                ## cat("PlanD\n")
                gamlss(formula = mu.planb,
                       sigma.formula = sigma.planb,
                       nu.formula = nu.planb,
                       tau.formula = tau.planb,
                       family = family.planb,
                       data = data,
                       control = gamlss.control(n.cyc = min(3, n.cyc), trace = trace , ...),
                       i.control = glim.control(bf.cyc = min(3, bf.cyc), cyc = min(2, cyc), ...))
              }
            )
          }
        )
      }
    )

    # Predict the parameters values for the units with missings
    capture.output(predictions <- predictAll(fit, new.data, type="response",
                                             data = data),
                   file = NULL)

    # Return wrapper that will call the generation function
    # corresponding to the distribution family of the gamlss
    # model
    function(...) {
      r <- paste("r", fit$family[1], sep = "")

      predictions$y <- NULL
      do.call(r,
              args = c(predictions,
                       n = length(predictions$mu)
                       )
              )
    }
  },
  error = function(e) {
    function(...) {do.call(rep, args = list(NA, nrow(new.data)))}
  })

}
