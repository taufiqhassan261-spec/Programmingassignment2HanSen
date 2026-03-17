## This code creates a matrix object that can cache (store) its inverse.
## The idea is to avoid recalculating the inverse multiple times,
## which can be slow for large matrices.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL  # this will store the inverse once it is computed
  
  # update the matrix and reset the cached inverse
  # (because the old inverse is no longer valid)
  setMatrix <- function(newMatrix) {
    x <<- newMatrix
    inverse <<- NULL
  }
  
  # return the current matrix
  getMatrix <- function() {
    x
  }
  
  # store the computed inverse
  setInverse <- function(inv) {
    inverse <<- inv
  }
  
  # return the cached inverse (if it exists)
  getInverse <- function() {
    inverse
  }
  
  # return all helper functions as a list
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function returns the inverse of the matrix.
## If the inverse was already computed before, it uses the cached value.
## Otherwise, it calculates the inverse and stores it for future use.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # check if inverse is already cached
  if (!is.null(inv)) {
    message("using cached inverse")
    return(inv)
  }
  
  # compute the inverse since it is not cached
  mat <- x$getMatrix()
  inv <- solve(mat, ...)
  
  # store the result so next time it doesn't need recomputing
  x$setInverse(inv)
  
  inv
}
