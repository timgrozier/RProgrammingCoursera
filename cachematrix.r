## Put comments here that give an overall description of what your
## functions do

## function creates a matrix
##"get" returns the matrix created
## "setInverse" either returns a null or is referred to by the "Cache Solve" function to replace the x$setInverse with the newly computed inverse. 
## list function at the end so I can use the $ operator.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## First part checks whether the inverse has already been calcuated (and retrieves the cache if it has been calculated)
## otherwise computes the inverse of the matrix.

cacheSolve <- function(x,...) {
    if(!is.null(inv)) {
        print("retrieving cache")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
