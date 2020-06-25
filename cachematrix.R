## The functions below can be used to create a special "matrix", compute the
## inverse of that special "matrix" and cache its inverse

## This function creates the special "matrix" mentioned above and cache its
## inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" created by the
## previous function. If the inverse has already been calculated and the
## "matrix" is still the same, the function retrieves the inverse from the
## cache

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message('Getting cached data.')
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

