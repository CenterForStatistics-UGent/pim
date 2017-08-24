# Internal functions
# 
# These are a number of convenience functions that are used internally
# 

# lpaste pastes names together as a "list" to be used in a message
# argument x: a character vector
.lpaste <- function(x){
  lx <- length(x)
  if(!is.character(x)){
    stop("lpaste can only paste character values.")
  } else if(lx < 2){
    return(x)
  } else if(lx == 2){
    return(paste(x,collapse=" and "))
  } else{
    tmp <- paste(x[-lx], collapse=", ")
    return(paste(tmp,x[lx],sep=" and "))
  }
}

# Checks whether x and y contain the same elements
.same.elements <- function(x,y){
  if(!is.vector(x) || !is.vector(y))
    stop("x and y should be vectors.")
  
  !any(
    match(x,y,0L) == 0L,
    match(y,x,0L) == 0L
    )
  
}

# Gets the classes of all objects in an environment
# Takes an environment as argument
.get.classes <- function(envir){
  all.classes <- lapply(mget(ls(envir),envir=envir),
         class)
  all.classes
}

# Checks the classes in an environment against a (named)
# vector or list with the classes mentioned.
# 
.same.classes <- function(envir,classes){
  all.classes <- .get.classes(envir)
  if(!is.list(classes)) classes <- as.list(classes)
  
  if(!is.null(names.classes <- names(classes))){
    if(!.same.elements(names.classes,names(all.classes))){
      stop("Names of classes don't match")
    } else {
      all.classes <- all.classes[names.classes]
    }
  }
  identical(all.classes, classes)
}

.equal.nobs <- function(envir){
  all.lengths <- sapply(ls(envir), function(i){
    NROW(get(i,envir=envir,inherits=FALSE))
  })
  length(unique(all.lengths)) <= 1
}

valid.classes <- function(x){
  if(is.list(x)) x <- sapply(x,`[`,1)
  all(match(x,.valids.pim,0L) > 0)
}

# Currently not used. valid.classes should do it
# Could be adapted 
is.variable <- function(x){
  is.vector(x) | inherits(x, 'factor')
}

# Converts a string to a language object for use in
# formula manipulation:
as.language <- function(x){
  eval(parse(text=paste("quote(",x,")")))
}

# Catches the warning in glm.fit abput non-integer successes
catch.noninteger.handler <- function(w)
  if( any (grepl("non-integer #successes in a binomial glm", w)))
    invokeRestart("muffleWarning")

# Removes parentheses from a character input
remove.pars <- function(x){
  gsub("\\w\\(.*\\)","",x)
}

# checks whether all objects have equal length
.equal.lengths <- function(...){
  allObjects <- list(...)
  allClass <- sapply(allObjects, class)
  if(length(unique(allClass)) != 1)
    stop('All objects should be of the same class')
  allLengths <- sapply(allObjects, length)
  length(unique(allLengths)) <= 1
}

# This one is copied from the stats package to make confint work
format.perc <- function (probs, digits) {
  paste(format(100 * probs, 
               trim = TRUE, 
               scientific = FALSE, 
               digits = digits), 
        "%")
}
  