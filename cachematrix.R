## Put comments here that give an overall description of what your
## functions do

## This function creates a special object that hold a matrix and possibly its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    #make sure the cache is clear
    m <- NULL
    #setter method. If the value of the matrix changes, reset cached inverse to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #simple getter. Returns the actual matrix.
    get <- function() x
    #store the value of the cached inverse
    setsolve <- function(solve) m <<- solve
    #return the value of the cached inverse
    getsolve <- function() m
    #Return the object(list) with 4 functions.
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function will return the inverse of the matrix. If the cached alue is available, it will be returned.
## Otherwise the inverse will be computed and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #get from cache
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #Cached value was not present. Compute value, store in cache and return it.
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
