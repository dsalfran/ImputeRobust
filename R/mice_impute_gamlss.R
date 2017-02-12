#' Multiple Imputation with Generalized Additive Models for Location,
#' Scale, and Shape
#'
#' @description Imputes univariate missing data using a generalized
#'   model for location, scale and shape.
#'
#' @param y Numeric vector with incomplete data.
#' @param ry Response pattern of 'y' ('TRUE'=observed,
#'   'FALSE'=missing).
#' @param x Design matrix with 'length(y)' rows and 'p' columns
#'   containing complete covariates.
#' @param fitted.gam A predefined bootstrap gamlss method returned by
#'   \code{fit.gamlss}. Mice by default refits the model with each
#'   imputation. The parameter is here for a future faster modified
#'   mice function.
#' @param EV Logical value to determine whether to correct or not
#'   extreme imputed values. This can arise due to too much
#'   flexibility of the gamlss model.
#' @param family Distribution family to be used by GAMLSS.
#' @param n.ind.par Number of parameters from the distribution family
#'   to be individually estimated.
#' @param gd.tol Global deviance tolerance level of GAMLSS.
#' @param lin.terms Character vector specifying which (if any)
#'   predictor variables should enter the model linearly.
#' @param forceNormal Flag that if set to 'TRUE' will use a normal
#'   family for the gamlss estimation as a last resource.
#' @param c.crit Convergence criterion for the GAMLSS fitting step.
#' @param n.cyc Maximum number of cycles in the GAMLSS algorithm.
#' @param trace Flag to print at each gamlss iteration (TRUE) or not
#'   (FALSE).
#'
#' @return Numeric vector with imputed values for missing \code{y}
#'   values
#'
#' @details Imputation of \code{y} using generalized additive models
#'   for location, scale, and shape. A model is fitted with the
#'   observed part of the data set. Then a bootstrap sample is
#'   generated and used to refit the model and generate imputations.
#'
#'   The function \code{fit.gamlss} handles the fitting and the
#'   bootstrap and returns a method to generated imputations. The
#'   function \code{mice.impute.gamlssBI} sets the distribution family
#'   to a binary distribution.
#'
#'   Being gamlss a flexible non parametric method, there may be
#'   problems with the fitting and imputation depending on the sample
#'   size. The imputation functions try to handle anomalies
#'   automatically, but results should be still inspected.
#'
#' @references de Jong, R., van Buuren, S. & Spiess, M. (2016)
#'   Multiple Imputation of Predictor Variables Using Generalized
#'   Additive Models. Communications in Statistics -- Simulation and
#'   Computation, 45(3), 968--985.
#'
#' @references de Jong, Roel. (2012). “Robust Multiple Imputation.” Universität
#' Hamburg. \url{http://ediss.sub.uni-hamburg.de/volltexte/2012/5971/}.
#'
#' @references Rigby, R. A., and Stasinopoulos,
#'   D. M. (2005). Generalized Additive Models for Location, Scale and
#'   Shape. Journal of the Royal Statistical Society: Series C
#'   (Applied Statistics) 54 (3): 507–54.
#'
#' @author Daniel Salfran \email{daniel.salfran@uni-hamburg.de}
#'
#' @examples
#'
#'
#' require(lattice)
#' # Create the imputed data sets
#' imputed.sets <- mice(sample.data,
#'                      method = c("gamlss", "gamlss",
#'                                 "gamlss", "gamlssBI", "gamlss"),
#'                      visitSequence = "monotone",
#'                      maxit = 1, seed = 97123)
#'
#' fit <- with(imputed.sets, lm(y ~ X.1 + X.2 + X.3 + X.4))
#' summary(pool(fit))
#'
#' stripplot(imputed.sets)
#'
#' @export
mice.impute.gamlss <- function(y, ry, x, fitted.gam = NULL, EV = TRUE, ...) {
  Call <- match.call(expand.dots = TRUE)

  if (is.null(fitted.gam)) fitted.gam <- do.call(fit.gamlss, as.list(Call)[-1])

  imputed.values <- fitted.gam()
  # Repeat the bootstrap step if there is a problem with the fitting
  # of gamlss
  if (sum(is.na(imputed.values)) == length(imputed.values)) {
    imputed.values <- fitted.gam()
  }

  if (EV) {
    if (!(sum(is.na(imputed.values)) == length(imputed.values))) {
      outliers <- getOutliers(imputed.values, rho = c(.3, .3), FLim = c(0.08, 0.92))
      nans <- which(is.nan(imputed.values))
      idx <- c(outliers$iLeft, outliers$iRight, nans)
      if (length(idx) != 0) {
        imputed.values[idx] <- NA
        y[!ry] <- imputed.values
        R = ry
        ry <- !is.na(y)
        new.values <- mice.impute.pmm(y, ry, x, ...)
        imputed.values[idx] <- new.values
      }
    }
  }

  return(imputed.values)
}

#' @describeIn mice.impute.gamlss mice.impute.gamlssBI
#' @export
mice.impute.gamlssBI <- function(y, ry, x, fitted.gam = NULL, EV = TRUE, ...) {
  Call <- match.call(expand.dots = TRUE)
  Call[["family"]] <- BI
  Call[["n.ind.par"]] <- 1

  if (is.null(fitted.gam)) fitted.gam <- do.call(fit.gamlss, as.list(Call)[-1])

  imputed.values <- fitted.gam()
  # Repeat the bootstrap step if there is a problem with the fitting
  # of gamlss
  if (sum(is.na(imputed.values)) == length(imputed.values)) {
    imputed.values <- fitted.gam()
  }

  if (EV) {
    if (!(sum(is.na(imputed.values)) == length(imputed.values))) {
      outliers <- getOutliers(imputed.values, rho = c(.3, .3), FLim = c(0.08, 0.92))
      nans <- which(is.nan(imputed.values))
      idx <- c(outliers$iLeft, outliers$iRight, nans)
      if (length(idx) != 0) {
        imputed.values[idx] <- NA
        y[!ry] <- imputed.values
        R = ry
        ry <- !is.na(y)
        new.values <- mice.impute.pmm(y, ry, x, ...)
        imputed.values[idx] <- new.values
      }
    }
  }

  return(imputed.values)
}

#' @describeIn mice.impute.gamlss fit.gamlss
#' @export
fit.gamlss <- function(y, ry, x, family = NO, n.ind.par = 2,
                       gd.tol = Inf, lin.terms = NULL,
                       forceNormal = FALSE, c.crit = 0.001,
                       n.cyc = 20, trace = FALSE, ...) {

  data <- data.frame(y, x)

  fit <- partial(ImpGamlssFit, family = family, n.ind.par = n.ind.par,
                 gd.tol = gd.tol, lin.terms = lin.terms,
                 forceNormal = forceNormal, c.crit = c.crit,
                 n.cyc = n.cyc, trace = trace)

  imp.method <- partial(ImpGamlssBootstrap, fit = fit, R = ry)
  f <- imp.method(incomplete.data = data, ...)

  return(f)
}
