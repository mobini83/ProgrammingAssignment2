## The function makeCacheMatrix creates a special matrix object 
## that can cache its inverse. The function cacheSolve computes 
## the inverse of the object created by the makeCacheMatrix.

## this function creates an object including a matrix and its cached inverse. 
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y){
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inversed) inverseMatrix <<- inversed
  getInverse <- function() inverseMatrix
  
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## this function checks to see if the inverse is chached 
## returns it, otherwise computes and returns it. 
cacheSolve <- function(x, ...) {
    inversed <- x$getInverse()
    ## if the inverse is already calculated return it.
    if(!is.null(inversed)){
      message("getting cached inverse matrix")
      return(inversed)
    }
    ## if the inverse is not calculated before, get x, 
    ## calculate the inverse, cache the inverse, and return it.
    my_matrix <- x$get()
    inversed <- solve(my_matrix)
    x$setInverse(inversed)
    inversed
}
