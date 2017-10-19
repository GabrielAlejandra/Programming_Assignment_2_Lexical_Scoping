## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
######################################################################
## makeCacheMatrix:
######################################################################

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

set <- function(y) {
  x <<- y
  inv <<- NULL
}


get <- function() x

setiInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv


list(
  set = set, 
  get = get,
  setInverse = setInverse,
  getInverse = getInverse)	
}
######################################################################
## cacheSolve:
######################################################################

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("Getting cached matrix")
    return(inv)
  }
  
  Math<- x$get()
  inv <- solve(Math, ...)
  
 
  x$setInverse(inv)
  inv    
}

