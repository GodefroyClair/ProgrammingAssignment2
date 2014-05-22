## This function creates a special "matrix" object that can cache its inverse. 
## the getters and setters nmethods help to set and get the matrix and its inverse
makeCacheMatrix<- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve<- function(mat, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- mat$getinverse()
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        data <- mat$get()
        invrs <- solve(data, ...)
        mat$setinverse(invrs)
        invrs
}
