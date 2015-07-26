# These functions allow efficient computation of a matrix inverse.
# They cache the inverse and will return the cached version
# instead of recomputing the inverse as long as the matrix does
# not change.

# This function makes an environment where we keep the inverse
# of the matrix cached. It provides functions to get and set
# both the matrix and the inverse.
# It returns a list of interface functions, which holds the
# given matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


# Uses the above cached matrix environment to hold the inverse
# of a matrix while that matrix doesn't change.
# If the matrix is changed, will recache the inverse.
# Returns the inverse of the matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, , ...)
  x$setInverse(inv)
  return(inv)
}
