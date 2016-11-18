## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly

## The function makeCacheMatrix creates a list that is able to cache the inverse of a matrix.
## First, the function set the value of the matrix. 
## Second, the function get the value of the matrix. 
## Third, the function set the value of the inverse matrix.
## Finally, the matris get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
                minv <- NULL
                set <- function(y) {
                        x <<- y
                        minv <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) minv <<- inverse
                getinverse <- function() minv
                list (set = set, get = get,
                      setinverse = setinverse, 
                      getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        cacheSolve <- function(x, ...) {
        minv <- x$getinverse()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinverse(minv)
        minv
}

