## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseM <- x$getInverse()
    if(!is.null(inverseM)) {
        message("getting cached data")
        return(inverseM)
    }
    
    inverseM <- solve(x$get(), ...)
    x$setInverse(inverseM)
    inverseM
}
