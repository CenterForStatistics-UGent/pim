#' Extract information from a pim.formula object
#' 
#' This group of functions provides an easy way to extract the
#' extra information saved in a \code{\link{pim.formula}} object. 
#' Take a look at the help page of \code{\link{pim.formula}} for
#' more information.
#' 
#' @param x an object of the class pim.formula
#' @param ... arguments passed to other methods
#' 
#' @seealso the class \code{\link{pim.formula-class}}
#' 
#' @examples 
#' data("FEVData")
#' # Create the "model frame"
#' FEVenv <- new.pim.env(FEVData, compare="unique")
#' 
#' # create the formula and bind it to the pim.environment.
#' FEVform <- new.pim.formula(
#'   Age ~ I(L(Height) - R(Height))  ,
#'   FEVenv
#' )
#' lhs(FEVform)
#' has.specials(FEVform)
#' penv(FEVform)
#' 
#' FEVform2 <- new.pim.formula(
#'   FEV ~ Height*Sex,
#'   FEVenv
#' )
#' 
#' has.specials(FEVform2)
#' terms(FEVform2)
#' 
#' @include pim.formula-class.R
#' @rdname getters-pim.formula
#' @aliases lhs, terms, has.specials
#' @seealso \code{\link{response}} for extracting the pseudoresponse
#' variable, \code{\link{model.matrix}} for extracting the design
#' matrix of pseudo-observations, \code{\link{formula}} for
#' extracting the \code{pim.formula} and \code{\link{penv}} for 
#' extracting the pim environment.
#' @export
#' @return \code{has.specials()}: a single \code{TRUE} or \code{FALSE}
#' value indicating whether the formula right-hand side contains any
#' special functions.
setGeneric("has.specials",
           function(x)standardGeneric("has.specials"))

#' @rdname getters-pim.formula
setMethod("has.specials",
          signature= "pim.formula",
          function(x) x@has.specials)

#' @rdname getters-pim.formula
#' @return \code{terms()}: the \code{\link[stats]{terms}} object
#' of the \code{pim.formula} object
#' @export
setGeneric("terms")

#' @rdname getters-pim.formula
setMethod("terms",
          signature= "pim.formula",
          function(x) x@terms)

#' @rdname getters-pim.formula
#' @export
#' @return \code{lhs()}: an object of class \code{call} containing 
#' the left hand side of the formula as used in the pim. 
setGeneric("lhs",
           function(x)standardGeneric("lhs"))

#' @rdname getters-pim.formula
setMethod("lhs",
          signature= "pim.formula",
          function(x) x@lhs)
