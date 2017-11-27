#' Multiple Imputation with Generalized Additive Models for Location,
#' Scale, and Shape.
#' 
#' @description Imputes univariate missing data using a generalized
#'   model for location, scale and shape.
#'
#' @param y Numeric vector with incomplete data.
#' @param ry Response pattern of 'y' ('TRUE'=observed,
#'   'FALSE'=missing).
#' @param x Design matrix with 'length(y)' rows and 'p' columns
#'   containing complete covariates.
#' @param family Distribution family to be used by GAMLSS. It defaults
#'   to NO but a range of families can be defined by calling the
#'   corresponding "gamlssFAMILY" method.
#' @param n.ind.par Number of parameters from the distribution family
#'   to be individually estimated.
#' @param gam.mod list with the parameters of the GAMLSS imputation
#'   model.
#' @param fitted.gam A predefined bootstrap gamlss method returned by
#'   \code{fit.gamlss}. Mice by default refits the model with each
#'   imputation. The parameter is here for a future faster modified
#'   mice function.
#' @param EV Logical value to determine whether to correct or not
#'   extreme imputed values. This can arise due to too much
#'   flexibility of the gamlss model.
#' @param ... extra arguments for the control of the gamlss fitting
#'   function
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
#'   bootstrap and returns a method to generated imputations.
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
#' require(lattice)
#' # Create the imputed data sets
#'
#' predMat <- matrix(rep(0,25), ncol = 5)
#' predMat[4,1] <- 1
#' predMat[4,5] <- 1
#' predMat[2,1] <- 1
#' predMat[2,5] <- 1
#' predMat[2,4] <- 1
#' predMat[3,1] <- 1
#' predMat[3,5] <- 1
#' predMat[3,4] <- 1
#' predMat[3,2] <- 1
#' imputed.sets <- mice(sample.data, m = 2,
#'                      method = c("", "gamlssPO",
#'                                 "gamlss", "gamlssBI", ""),
#'                      visitSequence = "monotone",
#'                      predictorMatrix = predMat,
#'                      maxit = 1, seed = 973,
#'                      n.cyc = 1, bf.cyc = 1,
#'                      cyc = 1)
#'
#' fit <- with(imputed.sets, lm(y ~ X.1 + X.2 + X.3 + X.4))
#' summary(pool(fit))
#'
#' stripplot(imputed.sets)
#'
#' @export
mice.impute.gamlss <- function(y, ry, x, family = NO, n.ind.par = 2,
                               fitted.gam = NULL, gam.mod = list(type = "pb"),
                               EV = TRUE, ...) {
  Call <- match.call(expand.dots = TRUE)

  if (is.null(fitted.gam)) fitted.gam <- do.call(fit.gamlss, as.list(Call)[-1])

  ## imputed.values <- do.call(fitted.gam, as.list(Call)[-1])
  imputed.values <- fitted.gam(...)
  # Repeat the bootstrap step if there is a problem with the fitting
  # of gamlss
  if (sum(is.na(imputed.values)) > 0) {
    imputed.values <- fitted.gam(...)
  }
  if (sum(is.na(imputed.values)) > 0) {
    imputed.values <- fitted.gam(...)
  }
  if (sum(is.na(imputed.values)) > 0) {
    imputed.values <- fitted.gam(...)
  }

  if (EV) {
    if (!(sum(is.na(imputed.values)) == length(imputed.values))) {
      outliers <- getOutliers(imputed.values, rho = c(.3, .3), FLim = c(0.15, 0.85))
      nans <- which(is.nan(imputed.values))
      idx <- c(outliers$iLeft, outliers$iRight, nans)
      if (length(idx) != 0) {
        imputed.values[idx] <- NA
        y[!ry] <- imputed.values
        R = ry
        ry <- !is.na(y)
        new.values <- mice.impute.midastouch(y, ry, x, ...)
        imputed.values[idx] <- new.values
      }
    }
  }

  return(imputed.values)
}

#' @rdname mice.impute.gamlss
#' @export
mice.impute.gamlssNO <- function(y, ry, x, fitted.gam = NULL, EV = TRUE, ...) {
  Call <- match.call(expand.dots = TRUE)
  Call[["family"]] <- NO

  return(do.call(mice.impute.gamlss, as.list(Call)[-1]))
}

#' @rdname mice.impute.gamlss
#' @export
mice.impute.gamlssBI <- function(y, ry, x, fitted.gam = NULL, EV = TRUE, ...) {
  Call <- match.call(expand.dots = TRUE)
  Call[["family"]] <- BI
  Call[["n.ind.par"]] <- 1

  return(do.call(mice.impute.gamlss, as.list(Call)[-1]))
}

#' @rdname mice.impute.gamlss
#' @export
mice.impute.gamlssJSU <- function(y, ry, x, fitted.gam = NULL, EV = TRUE, ...) {
  Call <- match.call(expand.dots = TRUE)
  Call[["family"]] <- JSU

  return(do.call(mice.impute.gamlss, as.list(Call)[-1]))
}

#' @rdname mice.impute.gamlss
#' @export
mice.impute.gamlssPO <- function(y, ry, x, fitted.gam = NULL, EV = TRUE, ...) {
  Call <- match.call(expand.dots = TRUE)
  Call[["family"]] <- PO

  return(do.call(mice.impute.gamlss, as.list(Call)[-1]))
}

#' @rdname mice.impute.gamlss
#' @export
mice.impute.gamlssTF <- function(y, ry, x, fitted.gam = NULL, EV = TRUE, ...) {
  Call <- match.call(expand.dots = TRUE)
  Call[["family"]] <- TF

  return(do.call(mice.impute.gamlss, as.list(Call)[-1]))
}

#' @rdname mice.impute.gamlss
#' @export
mice.impute.gamlssGA <- function(y, ry, x, fitted.gam = NULL, EV = TRUE, ...) {
  Call <- match.call(expand.dots = TRUE)
  Call[["family"]] <- GA

  return(do.call(mice.impute.gamlss, as.list(Call)[-1]))
}

#' @rdname mice.impute.gamlss
#' @export
mice.impute.gamlssZIBI <- function(y, ry, x, fitted.gam = NULL, EV = TRUE, ...) {
  Call <- match.call(expand.dots = TRUE)
  Call[["family"]] <- ZIBI

  return(do.call(mice.impute.gamlss, as.list(Call)[-1]))
}

#' @rdname mice.impute.gamlss
#' @export
mice.impute.gamlssZIP <- function(y, ry, x, fitted.gam = NULL, EV = TRUE, ...) {
  Call <- match.call(expand.dots = TRUE)
  Call[["family"]] <- ZIP

  return(do.call(mice.impute.gamlss, as.list(Call)[-1]))
}

#' @rdname mice.impute.gamlss
#' @export
fit.gamlss <- function(y, ry, x, family = NO, n.ind.par = 2,
                       gam.mod = list(type = "pb"), ...) {

  data <- data.frame(y, x)

  fit <- partial(ImpGamlssFit, family = family, n.ind.par = n.ind.par,
                 gam.mod = gam.mod)

  imp.method <- partial(ImpGamlssBootstrap, fit = fit, R = ry)
  f <- imp.method(incomplete.data = data, ...)

  return(f)
}
