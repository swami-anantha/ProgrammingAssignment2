## 
## Use the makeCacheMatrix function to create a cached inverse matrix object
## Use the cacheSolve function to get the inverse matrix from the cached inverse matrix object
##     When the cacheSolve function is run for the first time, it will calculate and
##     cache the inverse matrix value in the cached inverse matrix object
##     This cached inverse matrix value will be used the next time the cacheSolve function is called
##     with the relevant cached inverse matrix object
##


#
# makeCacheMatrix Function:
#    Builds a cached inverse matrix object for the matrix
#        that was passed in as argument
#    Returns the cached inverse matrix object
#
makeCacheMatrix <- function(x = matrix()) {
    inverseVal <- NULL
    set <- function(y) {
        x <<- y
        inverseVal <<- NULL
    }
    get <- function() x
    setInverse <- function(invVal) inverseVal <<- invVal
    getInverse <- function() inverseVal
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#
# cacheSolve Function:
#    Calculates the inverse matrix value for the cached object
#        that is passed in as argument
#    Does not run the inverse matrix calculation if
#        the cached object has the cached inverse value
#    Returns the inverse matrix value
#
cacheSolve <- function(x, ...) {
    inverseVal <- x$getInverse()
    if(!is.null(inverseVal)) {
        message("Getting cached inverse matrix value")
        return(inverseVal)
    }
    data <- x$get()
    inverseVal <- solve(data, ...)
    x$setInverse(inverseVal)
    inverseVal
}

