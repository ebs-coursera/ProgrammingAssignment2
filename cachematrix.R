## This package contains family of functions for working with matrices that
## can cache some computation results inside these matrices:
##
## a) makeCacheMatrix()
##    creates matrix that can cache its inverse
## b) cacheSolve()
##    computes matrix inverse trying to re-use previously computed value
##
## Example usage:
##   m <- matrix(rnorm(9),3,3)
##   inv <- cacheSolve(m)
##   inv <- cacheSolve(m)
##
## Second cacheSolve() invocation uses previously computed matrix inverse value.
##

## Creates a special "cacheable matrix" which is really a list containing
## methods to set/get matrix value, set/get matrix inverse.
## Matrix created by this function can be passed to cacheSolve().
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x

    setinverse <- function(val) inv <<- val
    getinverse <- function() inv

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function is similar to standard R solve() function but defined for
## special "cacheable matrix" which is created by makeCacheMatrix() defined
## above.
## Internally, when called to compute matrix inverse, it caches the result
## inside 'x' in order to speed up future calls for the same 'x' contents.
cacheSolve <- function(x, ...) {
    ## solve() can be used to compute more than just matrix inverse
    ## if additional parameters are specified.
    ## If this is the case, don't try to use/set cached inverse value.
    if (nargs() != 1)
        return(solve(x$get(), ...))

    inv <- x$getinverse()
    if (!is.null(inv))
        return(inv)

    inv <- solve(x$get(), ...)
    x$setinverse(inv)
    inv
}
