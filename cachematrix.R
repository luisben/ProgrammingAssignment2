## The following functions implement a special matrix structure that can
## keep a cached copy of its inverse. This structure provides get and set
## functions to update and obtain its current value, and setinverse and getinverse
## functions to update and obtain its inverse matrix.

## this function builds a special matrix structure with a cache that can store its inverse
## the cache is reset if the value of the matrix is changed.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function calculates the inverse of a given cacheMatrix. If a cached inverse matrix exists, it
## is returned, if not, then the inverse is calculated, stored in the cache of x, and returned

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("returning cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}