## The functions defined here cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
	  i <- NULL
        set <- function(n) {
                m <<- n
                i <<- NULL
        }
        get <- function() m
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'

	  i <- m$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- m$get()

	  ## Compute the inverse
        i <- solve(data, ...) 
        m$setinverse(i)
        i
}
