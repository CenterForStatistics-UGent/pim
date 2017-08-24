#' Create a poset function
#' 
#' This function creates a poset function from a poset. The function is not
#' exported and shouldn't be called by the user. 
#' 
#' @param poset a vector with the columns as indices
#' 
#' @return A function that takes a single vector as argument, and that
#' returns the vector with the poset vector applied to it.
#' 
#' 
#' 
#' @rdname make.posfun
#' @name .make.posfun
.make.posfun <- function(poset){
  # Sanity checks
  if(!is.numeric(poset))
    stop("poset has to be numeric")
  
  if(!is.vector(poset))
    stop("poset should either be a vector.")
  
  poset <- as.integer(poset)
  
  # return. This whole function is redundant actually...
  # Should be tackled
    function(x){
      x[poset]
    }  
  
  
}
