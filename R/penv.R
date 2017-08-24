#' Extract a pim environment from a model or formula
#' 
#' This function allows you to extract the 
#' \code{\link{pim.environment}} object from either a \code{pim}
#' object or a \code{pim.formula} object. 
#' 
#' @param x either a \code{pim} or a \code{pim.formula} object
#' 
#' @return In case of a \code{pim} object, the \code{pim.environment}
#' contained therein. In case of a \code{pim.formula} object, 
#' the environment itself. 
#' See the help page \code{pim.formula-class}.
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
#' theEnv <- penv(FEVform)
#' ls(theEnv)
#' 
#' themodel <- pim(Age ~ Height, FEVenv)
#' thePEnv <- penv(themodel)
#' thePEnv
#' 
#' ls(thePEnv)
#' # Note that this is a different environment, and that it only contains
#' # the variables in the formula, contrary to the environment created
#' # by new.pim.formula
#' @export
setGeneric("penv",
           function(x) standardGeneric("penv"))

#' @rdname penv
setMethod("penv",
          signature = "pim.formula",
          function(x) x@penv)

#' @rdname penv
setMethod("penv",
          signature = "pim",
          function(x) x@penv)
