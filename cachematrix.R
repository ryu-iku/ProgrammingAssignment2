## Caching the Inverse of a Matrix

##  This function creates a object that can cache the inverse of a matrix (x).
makeCacheMatrix <- function(x = matrix()) {
    inverseM <- NULL
    
    set <- function(value) {
        x <<- value
        inverseM <<- NULL
    }
    
    get <- function() x
    setInverse <- function(input) inverseM <<- input
    getInverse <- function() inverseM
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}

## This function computes the inverse matrix of a "makeCacheMatrix" object (x).
cacheSolve <- function(x, ...) {
    
    inverseM <- x$getInverse()
    
    if(!is.null(inverseM)) {
        message("getting cached data")
        return(inverseM)
    }
    
    inverseM <- solve(x$get(), ...)
    x$setInverse(inverseM)
    
    ## Return a matrix that is the inverse of the target matrix.
    inverseM
}
