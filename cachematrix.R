## These functions make a special matrix that can store its inverse
## so that we don't have to recalculate it every time.

## This function creates the special matrix object
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL  # store the inverse here
  
  # set a new matrix and clear the old inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the current matrix
  get <- function() x
  
  # store the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # get the stored inverse
  getinverse <- function() inv
  
  # return a list of the functions
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function computes the inverse of the special matrix
## If it's already cached, it just returns that instead of recalculating
cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  # return cached inverse if it exists
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculate the inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  
  # save the inverse for later
  x$setinverse(inv)
  
  inv
}
