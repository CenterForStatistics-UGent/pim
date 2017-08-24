#' Convert a pim.environment to a data frame
#' 
#' This function extracts all data from a \code{\link{pim.environment}}
#' and returns it as a data frame. Note that this is the original data
#' frame, not the one with pseudo observations.
#' 
#' TO DO: Insert link to how to get pseudo observations out.
#' 
#' @param x a \code{pim.environment} object
#' @param row.names NULL or a character vector giving the row names for
#' the data frame. Missing values are not allowed.
#' @param optional logical. if TRUE, setting row names and converting
#' column names (to syntactic names: see \code{\link{make.names}})
#' is optional
#' @param ... additional arguments to be passed to or from methods,
#' including \code{stringsAsFactors}. For more information, see the
#' function \code{\link[base]{as.data.frame}} from the base package.
#' 
#' @return a data frame.
#' 
#' @examples 
#' # Create a pim environment
#' data("DysData")
#' Dys <- new.pim.env(DysData)
#' str(as.data.frame(Dys))
#' 
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