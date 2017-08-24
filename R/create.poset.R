#' Create a poset 
#' 
#' This function creates a poset for use in a pim model based on a
#' number of observations and a comparison type. This function is
#' called from \code{\link{new.pim.poset}} and returns a list that
#' can be used as a value for its argument \code{compare}.
#' 
#' @param compare a character value, either 'unique' or 'all'
#' @param n an single integer value indicating how many observations there
#' are in the model.
#' @return A named list with 2 elements, called "L" and "R", containing
#'  the selection indices for the left hand and right hand side of a pim.
#' @examples
#' create.poset(n=10)
#' create.poset('all',n=4)
#' @export
create.poset <- function(compare=c('unique','all'),n){
  ind <- seq_len(n)
  unique <- match.arg(compare) == 'unique'
  out <- vector("list",length=2)
  names(out) <- c('L','R')
  
  if(unique){
    out$L <- rep(ind, times=(ind-1L)[n:1L])
    out$R <- unlist(lapply(ind[-1L],seq,n))
  } else {
    out$L <- rep(ind,each=n-1L)
    out$R <- unlist(lapply(ind,function(i)ind[-i]))
  }
  return(out)
}
