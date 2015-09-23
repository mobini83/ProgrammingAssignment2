## The function makeCacheMatrix creates a special matrix object 
## that can cache its inverse. The function cacheSolve computes 
## the inverse of the object created by the makeCacheMatrix.

## this function creates an object including a matrix and
## its cached inverse. It has 4 functions: set, get, setInverse,
## and getInverse.
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  ## substitutes the matrix stored matrix with input matrix y. 
  set <- function(y){
    x <<- y
    inverseMatrix <<- NULL
  }
  ## Returns the matrix x.
  get <- function() x
  
  ## sets the input "inversed" as the inverseMatrix
  setInverse <- function(inversed) inverseMatrix <<- inversed
  
  ## returns the inverseMatrix
  getInverse <- function() inverseMatrix
  
  ## store the four above functions in a list
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## this function checks to see if the inverse is chached 
## returns it, otherwise computes and returns it and caches 
## the coputed inverse. 
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
