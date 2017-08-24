#' Constructor for a pim.environment
#' 
#' This functions serves as a constructor for an object of the class
#' \code{\link{pim.environment}}. In most cases, calling this function directly
#' is not necessary.
#' 
#' @details This function is called during the preparation of the model
#' matrix for a pim. The resulting object is used to evaluate the formula
#' of a pim, and stores information on how this is done. 
#' 
#' Note that the parent of the environment is actually the 
#' \code{\link{pim.poset}} object in the \code{poset} slot. 
#' The parent you set using the \code{env} argument, 
#' is the parent of the \code{pim.poset} object.  This ensures that
#' when a formula is evaluated in the \code{pim.environment} it 
#' will use a suitable search path to find all functions and objects.
#' 
#' @param data a data frame, a list or an environment containing
#' the data for a probabilistic index model. 
#' @param compare a character vector, matrix or list that defines how the
#' set of pseudo observations (poset) should be constructed. 
#' if set to \code{NULL}, no poset is constructed.
#' See also \code{\link{new.pim.poset}} 
#' for more information on how to specify a custom poset.
#' @param env an environment that is the parent environment of the object.
#' @param vars An optional character vector with the names of the variables
#' that should be included in the pim environment. Note that the
#' variable names should be found in the object passed to argument \code{data}. 
#' @param classes An optional character vector with the classes of the 
#' variables in the environment, given in the same order as 
#' the argument \code{data.names}.
#' @param ... extra parameters for construction of the poset, like
#' the argument \code{compare} from \code{\link{new.pim.poset}}.
#' 
#' @return an object of the class \code{\link{pim.environment}}
#' @include pim.environment-class.R
#' @aliases new.pim.env
#' @examples
#' new.pim.env() # Creates an empty object
#' 
#' # Starting from a data frame
#' data(DysData)
#' env1 <- new.pim.env(DysData)
#' 
#' env2 <- new.pim.env(DysData, compare=NULL)
#' poset(env2)
#' env3 <- new.pim.env(DysData, compare="all")
#' poset(env3)
#' 
#' 
#' data(FEVData)
#' env4 <- new.pim.env(FEVData, vars=c('Age','Sex'))
#' ls(env4)
#' 
#' 
#' @export
setGeneric("new.pim.env",
           function(data, ...){
             standardGeneric("new.pim.env")
           })
#' @rdname new.pim.env
setMethod("new.pim.env",
          signature=c(data="missing"),
          function(data, ...){
            mc <- match.call()
            if(!is.null(mc$compare))
              stop("No data specified, poset cannot be constructed.")
            new("pim.environment")
          })

#' @rdname new.pim.env
setMethod("new.pim.env",
          signature=c(data="environment"),
          function(data, compare = "unique",
                   env=parent.frame(),
                   vars=NULL,
                   classes=NULL,...){
            
            dots <- match.call(expand.dots=FALSE)[['...']]
            if(match('nobs',names(dots),0L) >0L)
              warning('nobs argument is ignored.')
            
            out <- new("pim.environment")
            
            if(is.null(vars)){
              vars <- ls(data)
            } 
            # You can't just assign an environment to .xData
            # if happening with the global environment, this hangs R.

            out@.xData <- list2env(mget(vars,data))
            out@data.names <- vars
            out@classes <- .get.classes(out@.xData)
            out@nobs <- length(get(vars[1],envir=data,inherits=FALSE))
            
            # create poset
            if(!is.null(compare)){
              out@poset <- new.pim.poset(compare, nobs=out@nobs,
                                         parent=env,...)
              out@is.complete <- TRUE
            } else{
              out@is.complete <- FALSE
            }
           
            validObject(out)
            if (out@is.complete) parent.env(out) <- out@poset
            out
          })

#' @rdname new.pim.env
setMethod("new.pim.env",
          signature=c(data="list"),
          function(data,compare = "unique",vars=NULL,...){
            
            if(!is.null(vars)){
              data <- data[vars]
            }
            
            data.names <- names(data)
            classes <- sapply(data, function(i){
              inherits(i,.valids.pim)
            })
            
            if(!valid.classes(classes)){
              stop("Some list elements are of a wrong class.")
            }
            
            if(length(nobs <- unique(sapply(data,length)))!=1){
              stop("All elements in the list should have the same length")
            }
            .new.pim.env(data,
                         compare,
                         data.names=data.names,
                         classes=classes,
                         nobs=nobs,
                         ...)
          })

#' @rdname new.pim.env
setMethod("new.pim.env",
          signature=c(data="data.frame"),
          function(data,compare = "unique",vars=NULL,...){
            
            if(!is.null(vars)){
              data <- data[vars]
            } else {
              vars <- names(data)
            }
            
            .new.pim.env(data,
                         compare,
                         data.names=vars,
                         nobs=nrow(data),
                         ...)
          })
#' @rdname new.pim.env
setMethod('new.pim.env',
          signature = c(data = 'ANY'),
          function(data, ...){
            data <- tryCatch({
              as.data.frame(data)
            }, error = function(e) "Cannot convert data to a data frame.")
            
            new.pim.env(data, ...)
          })

# The function .new.pim.env : the actual workhorse.

.new.pim.env <- function(data,
                         compare = "unique",
                         env=parent.frame(),
                         data.names,
                         classes,
                         nobs,
                         ...){
  object <- new("pim.environment")
    
  if(missing(classes))
    classes <- sapply(data,class,simplify=FALSE)
  
  if(missing(data.names))
    data.names <- names(data)
  
  # Add information
  object@.xData <- as.environment(data)
  object@data.names <- data.names
  object@classes <- classes
  object@nobs <- nobs
    
  if(!is.null(compare) ){
    object@poset <- new.pim.poset(compare,nobs, parent = env)
    object@is.complete <- TRUE
    
  } else {
    object@is.complete <- FALSE
  }

  validObject(object)
  if(object@is.complete)  
    parent.env(object) <- object@poset
  
  object
  
}
