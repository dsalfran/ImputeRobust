#' Model creator
#'
#' @description This is a helper function to be used within the gamlss
#'   fitting procedure. It creates automatically a formula object for
#'   the variables named a given data frame. The dependent variable is
#'   the one in the first column and the rest are treated as
#'   independent.
#'
#' @param data Data frame that will provide the named variables.
#' @param type Type of model. Available choices are c("linear", "cs",
#'   "pb")
#' @param par Optional list parameter if the model is not linear
#' @param lin.terms Specify which predictors should be included
#'   linearly. For example, binary variables can be added directly as
#'   an additive term instead of defining a spline.
#'
#' @return Returns a formula object.
ModelCreator <- function(data, type, par = NULL, lin.terms = NULL){
  if (class(data) != "data.frame") {
    stop("'data' must be a data frame")
  }
  dependent <- names(data)[1]
  factors <- names(data)[-1]

  if (!is.null(lin.terms)) {
    idx <- match(lin.terms, factors)
    v <- vector(mode = "logical", length = length(factors))
    v[idx] = TRUE
    factors <- factors[!v]
  }

  if (type == "response") {
    response <- factors[length(factors)]
    formula <- as.formula(paste(dependent, "~", response))
    return(formula)
  }

  if (type == "p-response") {
    response <- factors[length(factors)]
    formula <- as.formula(paste(dependent, "~",
                                paste("pb(", response, ")")))
    return(formula)
  }

  if (type == "linear" && is.null(lin.terms)) {
    # Define a linear model
    formula <- as.formula(paste(paste(dependent, " ~ ", sep = ""),
                                paste(factors, collapse = " + ")))
  } else if (type == "linear" && !is.null(lin.terms)) {
    formula <- as.formula(paste(paste(dependent, " ~ ", sep = ""),
                                paste(factors, collapse = " + "), "+",
                                paste(lin.terms, collapse = "+")))
  } else if (type == "cs") {
    # Define a cubic spline model
    if (is.null(par)) {
      df = 3
    } else {
      df = par
    }
    if (is.null(lin.terms)) {
      formula <- as.formula(paste(
        paste(dependent, " ~ ", sep = ""),
        paste("cs(", factors, " , df = ", df, ")", sep = "",
              collapse = "+")))
    } else {
      formula <- as.formula(paste(
        paste(dependent, " ~ ", sep = ""),
        paste("cs(", factors, " , df = ", df, ")", sep = "",
              collapse = "+"), "+", paste(lin.terms, collapse = "+")))
    }
  } else if (type == "pb") {
    # Define a P-spline model
    if (!is.null(par)) {
      control = paste("control = pb.control(degree = ", par$degree,
                      ", order = ", par$order, ")", sep = "")
      if (is.null(lin.terms)) {
        formula <- as.formula(paste(
          paste(dependent, " ~ ", sep = ""),
          paste("pb(", factors, " ,", control, ")", sep = "",
                collapse = "+")))
      } else {
        formula <- as.formula(paste(
          paste(dependent, " ~ ", sep = ""),
          paste("pb(", factors, " ,", control, ")", sep = "",
                collapse = "+"), "+", paste(lin.terms, collapse = "+")))
      }
    } else {
      if (is.null(lin.terms)) {
        formula <- as.formula(paste(
          paste(dependent, " ~ ", sep = ""),
          paste("pb(", factors, ")", sep = "",
                collapse = "+")))
      } else {
        formula <- as.formula(paste(
          paste(dependent, " ~ ", sep = ""),
          paste("pb(", factors, ")", sep = "",
                collapse = "+"), "+", paste(lin.terms, collapse = "+")))
      }
    }
  }
  else {
    stop("Wrong choice of model")
  }

  return(formula)
}
