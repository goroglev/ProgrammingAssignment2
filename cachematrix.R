## Construct a cache matrix object with functions to get and set the matrix
## and its inverse. Provide a function to retrieve the inverse of a matrix
## from the cache if it's been already set, otherwise compute it first.

makeCacheMatrix <- function(x = matrix()) {
    ## Return a cache matrix object consisting of `get` & `set` functions 
    ## for the matrix itself and `getinverse` & `setinverse` functions
    ## for its inverse.
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    getinverse <- function() {
        inverse
    }
    setinverse <- function(inverse_) {
        inverse <<- inverse_
    }
    list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}

cacheSolve <- function(x, ...) {
    ## Return the inverse of a matrix from cache if it set,
    ## otherwise compute it and put it in cache before returning.
    if (!is.null(x$getinverse())) {
        message("Returning cached value of matrix")
        return(x$getinverse())
    }
    # compute the inverse of the matrix in object `x`
    inverse = solve(x$get(), ...)
    x$setinverse(inverse)
    
    inverse
}
