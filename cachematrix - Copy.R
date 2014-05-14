## cachematrix.R
## Contains two functions that work together to hold a matrix and a related matrix that is the
## result of an operation on the original matrix.

## Takes a square matrix and returns a list of getter/setter functions for the matrix passed in
## and a related, cached matrix
makeCacheMatrix <- function(x = matrix()) {
    # check for valid input
    if (is.null(x)) {
        stop("Matrix x is null")
    }
    if (class(x) != "matrix") {
        stop("x must be a square matrix")
    }
    if (ncol(x) != nrow(x)) {
        stop(
            paste("x matrix must be square (currently", nrow(x), "x", ncol(x), ")")
            )
    }
    
    ## variable to hold the related matrix (default to null)
    cached <- NULL
    ## setter function for the matrix
    set <- function(y) {
        x <<- y
        ## when the matrix is changed, clear the stored cached matrix
        ## (since it is for any prior stored matrix)
        cached <<- NULL
    }
    ## getter function for the matrix
    get <- function () x
    
    ## setter function for the cached matrix
    setCached <- function(matrixToCache) cached <<- matrixToCache
    ## getter function for the cached matrix
    getCached <- function() cached
    
    ## return a list containing getter/setter functions for the
    ## original matrix and the related cached matrix
    list(set = set, 
         get = get,
         setCached = setCached,
         getCached = getCached)
}


## Takes the function list returned by makeCacheMatrix and uses it to find the inverse of the
## matrix given to makeCacheMatrix. If the inverse has not been inverted and cached,
## it does that and returns the inverted matrix, otherwise the function simply returns the 
## inverted matrix that was cached by a prior call
cacheSolve <- function(x, ...) {
    ## check for valid input
    if (is.null(x)) {
        stop("CacheMatrix provided is null, this function expects the output of makeCacheMatrix")
    }
    ## make sure this list contains the expected functions
    funNames <- names(x)
    if (is.null(funNames) | !all(funNames == c("set","get","setCached","getCached"))) {
        stop("CacheMatrix provided is incorrect (does not contain all the expected functions created from makeCacheMatrix)")
    }
    ## for worst case should check that each field is a function with the right signature, but that's
    ## enough checking for now...
    
    ## lookup the cached matrix and return it if it exists
    m <- x$getCached()
    if (!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    ## otherwise, invert the original matrix...
    data <- x$get()
    m <- solve(data, ...)
    ## cache it for later retrieval...
    x$setCached(m)
    ## and return the inverted matrix
    m
}
