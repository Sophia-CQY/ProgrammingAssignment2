## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function( matrix ) {
        x <<- matrix
        m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse)  m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
       }
}


## The following function calculates the inverse of the special "matrix" created with the above function.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setInverse(m)
        m
}
