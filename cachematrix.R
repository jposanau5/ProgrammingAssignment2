## The makeCacheMatrix and cacheSolve functions provide a 
## "smart" matrix object and inversion function which caches the
## matrix inverse to avoid uneccessary computation on repeated
## calls, and does not cache the inverse until it is required.
## These functions assume the matrix is square and invertible.

## This function encapsulates values of a square invertible 
## matrix and its inverse as local environment variables, and 
## returns a list of functions to get and set these values.
## The matrix inverse is not calculated and returns null until set.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinvrs <- function(invrs) i <<- invrs
        getinvrs <- function() i
        list(set = set, get = get, 
             setinvrs = setinvrs,
             getinvrs = getinvrs)
}




## Return the inverse of a "cache-matrix" object created by makeCacheMatrix.
## If the "cache-matrix" inverse is already available, this value is returned;
## Otherwise the inverse is calculated and stored in the "cache-matrix".
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinvrs()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinvrs(i)
        i
}
