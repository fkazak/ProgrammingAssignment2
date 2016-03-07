## This file provides functions to create matrix capable of caching it's
## inverse matrix, and accessing inverse value

## makeCacheMatrix(x) creates common matrix wrapper, wrappers provides
## caching for inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
      x <<- y
      inverse <- NULL
    }
  
    get <- function() x
    
    setinverse <- function(inv) inverse <<- inv
    
    getinverse <- function() inverse
    
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve(x, ...)  returns the inverse of the wrapped matrix 
## passed as argument. It first attempts to retrieve cached inverse
## value for wrapped matrix passed as argument, if failed calculates 
## and caches the inverse.

cacheSolve <- function(x, ...) {
    result <- x$getinverse()
    if(is.null(result)) {
        result <- solve(x$get(),...)
        x$setinverse(result)
    }
    result
}
