## Goal of function below is to calculate matrix inversion.
## Matrix inversion is usually a costly computation so in code below caching feature is implemented.
## With the code below one can save computation time specially in case of inversions of big size matrixes

## If the result of last operation of matrix inversion can be reused then computations are not repeated - Computed result is delivered from cache
## In other case result's computed.

## Below is pair of functions that cache the result of inverse of a matrix.

## makeCacheMatrix(x=matrix())
## it creates object which includes matrix 'x' and it's inversion 'i'
## object have 4 methods:
##  - get() - gets as a result matrix 'x'
##  - set(y=matrix()) - assigns to matrix 'x' value of 'y'
##  - getinversion() - gets as a result 'i' - inversion of matrix 'x'
##  - setinversion(inversion=matrix()) assigns to matrix 'i' value of 'inversion'

makeCacheMatrix <- function(x = matrix()) {

## initiation of inversion matrix
    i <- NULL
    
## definition of set(y) method
## if the matrix 'x' have beedn changed then inversion matrix 'i' is reset to NULL value
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

## definition of get() method
    get <- function() x

## definition of setinversion(inversion) method
    setinversion <- function(inversion) i <<- inversion

## definition of getinversion() method
    getinversion <- function() i
    list(set = set, get = get,
         setinversion = setinversion,
         getinversion = getinversion)        
}


## cacheSolve(x=makeCascheMatrix(), ...)
## calculates Matrix inversion on object 'x' ('x' is makeCacheMatrix class)
## if result of last calculation of matrix inversion can be re-used then it's returend from the cache
## in other case it's calculated and returned

cacheSolve <- function(x, ...) {

## get actual value of inversion matrix
    i <- x$getinversion()

## check if actual inversion matrix is calculated. If it's calculated then return it and finish
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }

## if actual inversion matrix is not calculated then calculate it
    data <- x$get()

## matrix inversion calculation is done here:
    i <- solve(data, ...)

## makeCacheMatrix object is updated
    x$setinversion(i)

## Return a matrix that is the inverse of 'x'
    i
}
