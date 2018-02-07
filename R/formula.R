#' Extract the formula from a pim or pim.formula object
#'
#' This function allows you to extract a formula from a \code{\link{pim}}
#' or a \code{\link{pim.formula}} object. In the latter case, you extract
#' the original formula.
#'
#' This function is based on \code{\link[stats]{formula}} from the stats
#' package. It creates a generic and can hence be used more or less in the
#' same way. Yet, as the pim package is dependent on the correct
#' binding between the formula objects and different environments, it is
#' advised not to change the environments tied to the formulas and to
#' use this function only to extract the desired information.
#'
#' @param x a \code{pim} or \code{pim.formula} object
#' @param orig a locigal value indicating whether the original formula
#' (\code{TRUE}) or the \code{pim.formula} object (\code{FALSE}) should
#' be returned. Defaults to \code{FALSE}
#' @param ... arguments passed to other methods
#'
#' @return a \code{\link{pim.formula}} if \code{x} is a \code{pim}
#' object and \code{orig = TRUE}. Otherwise a \code{\link[stats]{formula}} object.
#'
#' @seealso \code{\link{pim.formula-class}} and \code{\link{pim-class}} for
#' more information on the classes.
#'
#' @examples
#' data("DysData")
#' themodel <- pim(SPC_D2 ~ Chemo, data = DysData)
#'
#'
#' ## This gives the pim.formula object
#' thepimform <- formula(themodel)
#' ## This extracts the original formula from the pim.formula object
#' formula(thepimform)
#' ## Alternatively you can use orig = TRUE to get the original formula
#' formula(themodel, orig = TRUE)
#'
#' @rdname formula
#' @include pim-class.R
#' @export
setGeneric("formula")


formula.pim <- function(x, orig=FALSE, ...){
  if(orig) formula(x@formula) else x@formula
}

#' @rdname formula
setMethod("formula",
          signature="pim",
          formula.pim)


formula.pim.formula <- function(x, ...){
  x@orig
}

#' @rdname formula
setMethod("formula",
          signature="pim.formula",
          formula.pim.formula)
