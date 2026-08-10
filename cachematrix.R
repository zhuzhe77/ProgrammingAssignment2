## Creates a special matrix object that stores a matrix
## and can cache its inverse using lexical scoping.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    get <- function() x

    setinverse <- function(inverse) {
        m <<- inverse
    }

    getinverse <- function() m

    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Computes the inverse of the matrix. If the inverse has
## already been calculated, retrieves it from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()

    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    data <- x$get()
    m <- solve(data, ...)

    x$setinverse(m)
    m
}


