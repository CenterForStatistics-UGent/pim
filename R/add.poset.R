#' Add a poset to a pim.environment object
#' 
#' This function adds a poset to a \code{\link{pim.environment}}
#' object.
#' 
#' @param x a pim.environment object
#' @param overwrite a logical value indicating whether the poset
#' should be overwritten if it's already present. Defaults to 
#' \code{FALSE} to avoid problems.
#' @param ... further parameters passed to \code{\link{new.pim.poset}}.
#' 
#' @section Warning: Although it might be tempting to pass the argument
#' \code{nobs} to \code{\link{new.pim.poset}}, you shouldn't. 
#' The necessary information is taken from the respective slot 
#' in the \code{pim.environment} object.
#' 
#' If you provide a matrix or a list as value for the argument \code{compare},
#' note that you can easily create a poset that doesn't use all the 
#' observations. This might or might not be your intention. If the poset
#' you try to create contains indices that go beyond the number of
#' observations, you will get errors.
#' 
#' @return The object with a (new) poset attached.
#' @seealso \code{\link{new.pim.poset}} for the possible values of the
#' arguments \code{compare} and \code{nobs}.
#' 
#' @examples
#' 
#' data(DysData)
#' Dysenv <- new.pim.env(DysData)
#' Dysenv
#' DysenvAll <- add.poset(Dysenv, overwrite = TRUE, 
#'                        compare = 'all', nobs = nobs(DysData))
#' compare(Dysenv)
#' compare(DysenvAll)                       
#' 
#' 
#' @include pim.environment-class.R
#' @export 
setGeneric('add.poset',function(x,...) standardGeneric('add.poset'))

#' @rdname add.poset
setMethod('add.poset',
          signature=c('pim.environment'),
          function(x,overwrite=FALSE,...){
            if(is.complete(x) & !overwrite){
              stop('pim.environment has already a poset attached. Set overwrite to FALSE if you want to replace the current one.')
            }
            x@poset <- new.pim.poset(...)
            
            if(x@poset@nobs > x@nobs)
              stop('poset contains indices larger than the number of observations')
            parent.env(x) <- x@poset
            x@is.complete <- TRUE
            
            x
          })
