## Put comments here that give an overall description of what your
## functions do
## My functions compute and cache the inverse of a matrix. 

## Write a short comment describing this function
## In makeCacheMatrix we create a getter and setter for the matrix we want to cache as well as we create getInverse and setInverse to cache the inverse of the matrix when it is calculated.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Write a short comment describing this function
## In cacheSolve we try to get the inverse of the matrix first and check if it is already cached in the system. If it is cached we return the cached value, otherwise we compute the inverse using solve(data, ...) then cache that value and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
