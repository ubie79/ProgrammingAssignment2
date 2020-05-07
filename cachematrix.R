## Inversion of matrix usually take a log period of time to compute.
## By caching the operation it will reduce the time to compute
## Below are two function implementing inverse technique by storing
## the matrix and do the inversion.

## This function is build to caching the inversion of matrix for 
## later usage

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## This function cimpute the cache of inversion from previous
## function (makeCacheMatrix). If the inversion is calculated and no
## changes happens in the matrix provided, it can retrieve the
## the inverse from the cache stored before.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    print("Getting cached data")
    return(inv)
  }
  mat <- x$get()
  
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
