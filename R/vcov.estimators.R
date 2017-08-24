#' vcov estimators for pim
#' 
#' \code{sandwich.vcov} and \code{score.vcov} are two similar estimators
#' for the variance-covariance matrix of a probabilistic index model.
#' These functions are meant to be used within a call to \code{\link{pim}}
#' as a value for the argument \code{vcov}.
#' 
#' You can create your own estimating functions for the variance-covariance
#' matrix. To do so, you have to make sure that your function allows for
#' the exact same arguments. As the function \code{pim.fit} calculates the
#' fitted values already, there's no need to incorporate the calculation of
#' these inside the function.
#' 
#' @note You should only use \code{score.vcov} in combination with an 
#' identity link
#' 
#' @param fitted The fitted values (calculated as \code{X \%*\% coef} with
#' \code{X} the design matrix and \code{coef} the coefficients)
#' @param X the design matrix
#' @param Y a numeric vector with pseudoresponses
#' @param W a numeric vector with weights. If weights are not applicable,
#' set to \code{NULL} (the default)
#' @param link a character vector with the link function
#' @param poset a list with the left and right indices. See \code{\link{poset}}
#' for more information.
#' @param ... arguments passed to downstream methods.
#' 
#' @return the variance-covariance matrix
#' 
#' @rdname vcov.estimators
#' @name vcov.estimators
#' @aliases sandwich.vcov score.vcov
#' @seealso \code{\link{sandwich.estimator}} for more information on the
#' actual fitting process. \code{\link{pim}} for a few examples in how
#' these are used
#' @export sandwich.vcov
sandwich.vcov <- function(fitted, X, Y, W, link, poset, ...){
  Ulist <- U.sandwich(fitted, X, Y, link, W)
  Ulist$g1 <- poset[[1]] 
  Ulist$g2 <- poset[[2]]
  fv <- Ulist$fv
  Ulist$fv <- NULL
  do.call(sandwich.estimator, Ulist)
  
}

#' @rdname vcov.estimators
#' @name vcov.estimators
#' @export score.vcov
score.vcov <- function(fitted, X, Y, W, link, poset, ...){
  Ulist <- U.score(fitted, X, Y, link, W)
  Ulist$g1 <- poset[[1]]
  Ulist$g2 <- poset[[2]]
  fv <- Ulist$fv
  Ulist$fv <- NULL
  do.call(sandwich.estimator, Ulist) 
}
