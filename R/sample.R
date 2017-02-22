#' Sample data set with a monotone missing pattern
#'
#' A simple data set with monotone missing pattern
#'
#' Sample data set with four predictors and a dependent variable. A
#' missing monotone pattern was generated in three predictors to
#' illustrate the gamlss imputation method.
#'
#' For the data generation process a parameter beta equal to
#' \code{c(1.3, .8, 1.5, 2.5)} and a predictor matrix \code{X <-
#' cbind(X.1, X.2, X.3, X.4)} are defined. Then, the sample data set
#' is created with the model \code{y ~ X.1 + X.2 + X.3 + X.4}.
#'
#' @name sample.data
#' @docType data
#' @format A data frame with 250 rows on the following 5 variables
#'   \describe{
#' \item{X.1}{Numeric variable from a normal distribution}
#' \item{X.2}{Count data from a Poisson distribution}
#' \item{X.3}{Numeric variable from a normal distribution}
#' \item{X.4}{Binary variable from a Binomial distribution}
#' \item{y}{Response variable}
#' }
#' @keywords datasets
#'
#'
#' @examples
#'
#' \donttest{
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
#' }
#' 
NULL
