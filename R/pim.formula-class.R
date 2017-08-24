#' Class pim.formula
#' 
#' This class contains information on the formula passed in 
#' a call to \code{\link{pim}}. The object is used to create the 
#' model matrix of a pim (see \code{\link{model.matrix}})
#' 
#' @section Note: 
#' This class is not exported, so it can't be extended as for now. 
#' Although it is possible to use the function \code{\link{new}} for
#' creation of new instances, users are strongly advised to use
#' the function \code{\link{new.pim.formula}} in case they need to manually
#' create a new instance of the class \code{pim.formula}.
#' 
#' @slot terms a \code{\link[stats]{terms.object}} derived from the formula
#' 
#' @slot has.specials a logical value indicating whether the right-hand
#' side of the original formula contains special functions like 
#' \code{\link{L}} and \code{\link{R}}
#' 
#' @slot has.lhs.fun a logical value indicating whether the left-hand
#' side of the original formula contains special functions. These 
#' exclude the functions \code{\link{P}} and \code{\link{PO}} but 
#' include functions like \code{\link[survival]{Surv}}. See Details
#' 
#' @slot predictors a character vector with the names of all the variables
#' mentioned in the right-hand side of the formula.
#' 
#' @slot response an character vector with the name of the response
#' variable.
#' 
#' @slot lhs a call with the processed left-hand side of
#' the formula
#' 
#' @slot orig a formula object with the original formula
#' 
#' @slot penv an \code{environment} object to which the
#' formula is related (i.e. the environment containing possible
#' \code{L} and \code{R} function definitions.) See Details. 
#' 
#' @slot has.intercept a logical value indicating whether the formula has
#' an explicit intercept (indicated by + 1)
#' 
#' @details Although a future version of this package will include the
#' possibility to fit survival models, this is currently not implemented.
#' If the \code{\link{pim}} function encounters special functions on the
#' left-hand side (i.e. when \code{has.lhs.fun} is \code{TRUE}), the
#' model won't be calculated.
#' 
#' The slot \code{penv} contains a reference to an environment
#' In most cases, this will be the environment contained in a 
#' \code{\link{pim.environment}} object. Note though that the
#' \code{pim.formula} object only contains a link to the environment.
#' The extra slots contained in the \code{pim.environment} object
#' are NOT copied to the \code{pim.formula}. Also keep
#' in mind that the environment linked to the \code{pim.environment}
#' object will continue to exist even after deleting the 
#' \code{pim.environment} itself, and this for as long as the 
#' \code{pim.formula} object exists.
#' 
#' This class is not exported and hence cannot be extended. It serves
#' internal use in the pim package only.
#'
#' @aliases pim.formula
#' @include pim-package.R
#' @include pim.environment-class.R
setClass(
  "pim.formula",
  slots=c(terms = "terms",
          has.specials = "logical",
          has.lhs.fun = "logical",
          predictors = "character",
          response = "character",
          lhs = "call",
          orig = "formula",
          penv = "environment",
          has.intercept = "logical"
          )
  )
