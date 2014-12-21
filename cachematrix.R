## This R script implements a pair of functions that cache the inverse of a matrix.
## As matrix inversion is usually a costly computation, the inverse of a matrix is
## cached rather than re-computed repeatedly (unless the matrix is changed). 


## This function creates a special "matrix" object that can cache its inverse.
## The matrix itself is stored in the variable x
## and its inverse is tored in the vairable inv after it has been calculated.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(newInv) inv <<- newInv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- inv(data, ...)
    x$setinv(inv)
    inv
}
