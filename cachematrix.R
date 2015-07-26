## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly.
## This pair of functions will cache the inverse of a matrix.
## 

makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve 
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` will retrieve the inverse from the cache.
    
        m <- x$getsolve()
        if(!is.null(m)) {
          message("getting cached data")
          return(m) 
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
