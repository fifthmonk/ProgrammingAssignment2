## R programming
## Programming Assignment 2

## makeCacheMatrix takes a matrix as an argument and return a list
## containing functions for retrieving the argument and its inverse if 
## it exists otherwise return a null vaule. The list also contains
## functions for caching the inverse of the argument 'x'.

makeCacheMatrix <- function(x = matrix()) {
        # initial variable im (the inverse of 'x') as null
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) im <<- solve
        getsolve <- function() im
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve takes return value from makeCacheMatrix as an argument
## and return an inverse of the argument. Extra arguments can be passed 
## to the function for calculation. If the inverse has already been calculated,
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix this is the inverse of 'x'
        im <- x$getsolve()
        # check if the inverse of 'x' does exist, return it
        if(!is.null(im)) {
                message("getting cached inverse")
                return(im)
        }
        # if the inverse of 'x' does not exist
        data <- x$get()
        # use the solve function to 
        im <- solve(data, ...)
        x$setsolve(im)
        im
}
