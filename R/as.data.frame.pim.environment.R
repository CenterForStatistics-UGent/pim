#' Convert a pim.environment to a data frame
#'
#' This function extracts all data from a \code{\link{pim.environment}}
#' and returns it as a data frame. Note that this is the original data
#' frame, not the one with pseudo observations.
#'
#' This function converts the environment first to a list, and then
#' converts this list using \code{\link[base]{as.data.frame}}. Hence
#' all arguments from the list method of \code{as.data.frame} can be
#' used in this function as well.
#'
#' Keep in mind that it doesn't make sense to try to extract the
#' pseudo observations from a pim environment. These observations are
#' determined by the formula, and hence extracting them only makes sense
#' when a formula is known.
#'
#' @seealso \code{\link{model.matrix}} for extracting the pseudo observations
#' from a pim model or based on a formula.
#'
#' \code{\link{pim.environment}} for more information on the class.
#'
#' @param x a \code{pim.environment} object
#' @param row.names NULL or a character vector giving the row names for
#' the data frame. Missing values are not allowed.
#' @param optional logical. if TRUE, setting row names and converting
#' column names (to syntactic names: see \code{\link{make.names}})
#' is optional
#' @param ... additional arguments to be passed to other methods,
#' including the argument \code{stringsAsFactors}. For more information, see the
#' function \code{\link[base]{as.data.frame}} from the base package.
#'
#' @return a data frame with the data in the pim environment.
#'
#' @examples
#' # Create a pim environment
#' data("DysData")
#' Dys <- new.pim.env(DysData)
#' str(as.data.frame(Dys))
#'
#' @aliases as.data.frame.pim.environment
#' @include pim.environment-class.R
#' @export
setGeneric("as.data.frame")

as.data.frame.pim.environment <-
  function(x,...){
  as.data.frame(as.list(x), ...)
}

#' @rdname as.data.frame
#' @export
setMethod("as.data.frame","pim.environment",
          as.data.frame.pim.environment)
