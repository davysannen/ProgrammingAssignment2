## Two functions are defined below for creating an object which allows storing a
## matrix and caching its inverse.

## The 'makeCacheMatrix' function creates a special "matrix" object, essentially
## by creating a list of functions to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse 
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse to NULL
  inv <- NULL
  
  # The function to set the value of the matrix
  set <- function(y) {
    # Set the value of x in the parent frame
    x <<- y
    # Set the value of the inverse in the parent frame to NULL
    inv <<- NULL
  }
  
  # The function to get the value of the matrix
  get <- function() x
  
  # The function to set the value of the inverse
  setInverse <- function(solved) inv <<- solved
  
  # The function to get the value of the inverse
  getInverse <- function() inv
  
  # Return the list of functions defined above
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The 'cacheSolve' function calculates the inverse of the special "matrix"
## object created by the makeCacheMatrix function. It first checks whether the
## inverse has already been calculated. If this is the case, it does not
## recompute it, but rather gets the inverse from the cache. If this is not the
## case, it calculates the inverse of the matrix and sets the value of the
## inverse in the cache. The assumption is made that the supplied matrix is
## always invertible.
cacheSolve <- function(x, ...) {
  # Try to get the inverse from the cache
  inv <- x$getInverse()
  
  # If the inverse was already calculated, print a message that this is the case
  # and return the inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse was not calculated yet, get the matrix stored in 'x'
  data <- x$get()
  # Compute the inverse of the matrix
  inv <- solve(data, ...)
  # Store the value of the inverse
  x$setInverse(inv)
  
  # Return a matrix that is the inverse of 'x'
  inv
}
