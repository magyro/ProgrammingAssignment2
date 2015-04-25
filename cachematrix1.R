options(digits=3) # number of digits to print on output 


## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly
## that pair of functions  caches the inverse of a matrix.

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

## X is the matrix to be inverted
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        ## reset the cash every time the "matrix" is build for new data
        m <<- NULL
    }

    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve  retrieves the inverse from the cache.
## prerequisite for that function to work correctly is to activate makeCacheMatrix with the matrix intended to inverse
## the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        ## get inverse from cache
        return(m)
    }
    ## new matrix, needs calculation
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
