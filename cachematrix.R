## Script for Programming Assignment 2
## Caching inverse of matrix
## Provide singular matrix to test and call $reset(anyvalue) to rest m

## this method stores the value in cache


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
#call this method to reset m
        reset <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(value) m <<- value

        getinverse <- function() m
        list(reset = reset , get = get,
             setinverse = setinverse ,
             getinverse = getinverse )
}

## this method computes inverse of a singular matrix
## stores the inverse in cache, upon consecutive call values are fetched from cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setinverse(m)
        m
}
