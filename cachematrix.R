## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function creates a special "matrix" containing a functions to the following:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <<- NULL
  
  # Set the value of the matrix.
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Get the value of the matrix.
  get <- function() {
    x
  }
  
  # Set the value/cache of the inverse of the matrix.
  setInverse <- function(solve) {
    inverse <<- solve
  }
  
  # Get the value/cache of the inverse of the matrix.
  getInverse <- function() {
    inverse
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function
## The following function calculates the inverse of the special "matrix" created with the above function.
## If it has been calculated before, the inverse is taken from the cache. Else, it is computed.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  ## Checks if inverse exists in cache. If yes, return inverse.
  if (!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  
  ## Else, compute and set inverse.
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
  
}
