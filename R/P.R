#' Probability function
#' 
#' This functions transform a comparison or otherwise
#' logical value to a numeric value for use in a pim.
#' 
#' These functions are constructed purely for notation. 
#' \code{P} is completely equivalent
#' to \code{\link{as.numeric}}, apart from an extra control
#' to check whether it actually makes sense to do so. 
#' The function \code{PO} is just short for \code{P(x < y) + 0.5*P(x == y)}
#' 
#' @param x for \code{P}, a logical value. For \code{PO} a numeric value.
#' @param y a numeric value or \code{NULL}. If \code{NULL}, the function
#' will try to calculate \code{PO(L(x),R(x))}, provided the functions
#' \code{\link{L}} and \code{\link{R}} are defined correctly. This
#' is the case when \code{PO} is used in the context of a probabilistic
#' index model fitted with \code{\link{pim}}.
#' 
#' @return A numeric value of 0, 0.5 or 1. 1 if x < y, 0.5 if x == y 
#' and 0 if x > y
#' 
#' @seealso \code{\link{pim}} and \code{\link{pim.formula}} for more information
#' on how this is used inside a pim context.
#' @examples
#' # Check in pim
#' 
#' @aliases PO
#' @export
P <- function(x){
  if(is.logical(x)) as.numeric(x) else 
    stop("P requires logical values.")
}

#' @rdname P
#' @export
PO <- function(x,y=NULL){
  if(is.null(y)) PO(L(x),R(x)) else
  P(x < y) + 0.5*P(x == y)
}
