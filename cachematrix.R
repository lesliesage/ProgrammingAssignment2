## This pair of functions computes the inverse of matrix 'x' and caches it, 
## then either gets and returns the cached copy or computes the inverse of 'x'.

## This function computes and caches the inverse of matrix 'x'. 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function first tries to return a cached copy of the inverse of 
## matrix 'x'. If there is no cached copy, it computes the inverse of 'x'.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
