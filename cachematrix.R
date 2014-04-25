## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#################################################################
## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # Creates a matrix that can cache its inverse
    #
    # Args:
    #   x:  A matrix used to initialize the object. If null
    #       or not passed, an empty matrix will be used.
    # Returns:
    #   A matrix like object that can cache its inverse
    
    # used to cache the inverse, once computed
    inv <- NULL
    
    # sets the matrix value and removes the inverse
    # from the cache (changing the matrix, the inverse
    # must be recalculated)
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # return the current value of the matrix
    get <- function() x
    
    # caches the value of the inverse matrix
    setInv <- function(inverse) inv <<- inverse
    
    # returns the inverse matrix if cached, NULL otherwise
    getInv <- function() inv
    
    # return a list of the methods to be used to
    # set and get the matrix value and the cached inverse
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function

#################################################################
## This function returns the inverse of a cacheMatrix getting
## it from the cache if available or computing it through the
## solve function if it has not been computed before

cacheSolve <- function(x, ...) {
    # Returns the inverse of the cacheMatrix x, getting it from
    #   the cahce, if available
    #
    # Args:
    #   x:  A cacheMatrix object.
    #   ... further arguments passed to solve()
    #
    # Returns:
    #   A matrix that is the inverse of x
    
    # gets the inverse from cache
    inv <- x$getInv()
    
    # if available, returns the cache value
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # gets the matrix inside the cachedMatrix
    data <- x$get()
    # computes the inverse
    inv <- solve(data, ...)
    # stored the computed inverse in cache
    x$setInv(inv)
    # returns the inverse
    inv
}
