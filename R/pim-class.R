#' Class pim
#' 
#' This class contains the fitting information resulting from a call to
#' \code{\link{pim}}. 
#' 
#' @slot formula The \code{\link{pim.formula}} object used in the fit
#' @slot coef a numeric vector with the fitted coefficients
#' @slot vcov a numeric matrix containing the variance-covariance matrix 
#' of the fitted coefficients
#' @slot penv a \code{\link{pim.environment}} object containing the 
#' data used to fit this 
#' @slot fitted a numeric vector containing the raw fitted 
#' @slot link a character vector describing the used link function
#' @slot estimators a list with the elements \code{coef} and \code{vcov},
#' containing either a character value with the name of the used estimator,
#' or the function itself.
#' @slot model.matrix If \code{keep.data} is set to \code{TRUE} 
#' while calling \code{\link{pim}} the original model matrix. 
#' Otherwise an empty matrix with 0 rows and columns.
#' @slot response If \code{keep.data} is set to \code{TRUE} 
#' while calling \code{\link{pim}} the original response vector.
#' Otherwise an empty numeric vector. 
#' @slot keep.data a logical value indicating whether the original
#' data is kept in the object. This is set using the argument
#' \code{keep.data} of the function \code{\link{pim}}.
#' \code{model} a character value with the value "difference",
#' "marginal", "regular" or "customized", indicating which
#' type of pim model has been fitted.
#' @include pim.formula-class.R pim.environment-class.R
setClass(
  'pim',
  slots=c(formula='pim.formula',
          coef = 'numeric',
          vcov = 'matrix',
          penv = 'pim.environment',
          fitted = 'numeric',
          link = 'character',
          estimators = 'list',
          model.matrix = 'matrix',
          response = 'numeric',
          na.action = 'character',
          keep.data = 'logical',
          model = 'character'),
  validity=function(object){
    if(any(names(object@estimators) != c('coef','vcov'))){
      "The list of estimators is malformed"
    } else if(!length(object@model)){
      "The slot model can't be empty"
    } else if(!object@model %in% c("difference","marginal",
                            "regular","customized")){
      "The value of model is not valid"
    } else {
      TRUE
    }
  }
  )

