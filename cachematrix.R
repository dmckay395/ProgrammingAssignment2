## Matrix inversion is usually a costly computation. makeCacheMatrix and 
## cacheSolve cache the inverse of a matrix rather than computing it.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatinv <- function(solve) m <<- solve
        getmatinv <- function() m
        list(set = set, get = get,
             setmatinv = setmatinv,
             getmatinv = getmatinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- solve(x)
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatinv(m)
        m
        
}