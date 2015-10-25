## This module contains a pair of functions that allows to cache  matrix
## inversion operation.

## Sample how to use:

## Creates inversed matrix
## Returns a list of functions for get/set original and invertex matrixes.

makeCacheMatrix <- function(x = matrix()){
  
  # init internal as NULL
  inversed_x <- NULL
  
  # internal functions:
  
  # sets original matrix and clears internal inversed_x
  set <- function(y) {
    x <<- y
    
    # previous value is obsolete
    inversed_x <<- NULL
  }
  
  # returns original matrix
  get <- function(){
    x
  }
  
  # sets inversed matrix
  set_inversed <- function(inversed){
    inversed_x <<- inversed
  }
  
  # returns inversed matrix
  get_inversed <- function(){
    inversed_x
  }
  
  # save into a list
  list(set = set,
       get = get,
       set_inversed = set_inversed,
       get_inversed = get_inversed)
}


## Returns a matrix that is the inverse of 'x'
## If inversion is already calculated, returns it,
## otherwise calculates and caches the value.

cacheSolve <- function(x, ...) {
  
  inversed_x <- x$get_inversed()
  
  # if inversed matrix has been calculated already, just return it
  if(!is.null(inversed_x)) {
	
    message("get cached value")
    return(inversed_x)
  }
  
  # if we are here, then has no result yet
  inversed_x <- solve(x$get(), ...)
  
  # cache the result
  x$set_inversed(inversed_x)
  
  # return the result
  inversed_x
}
