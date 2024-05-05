## This an assignment for a Coursera course on Data Science.
## I had to revise what inverse matrices are. Reference: https://www.mathsisfun.com/algebra/matrix-inverse.html

## The makeCacheMatric function creates a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    k <- NULL
    set <- function(y) {
        x <<- y
        k <<- NULL 
    }
    get <- function() x
    setinv <- function(inv) k <<- inv
    getinv <- function() k
    
    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
    )
}


## The cacheSolve function computes the inverse of the special 'matrix' returned by the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
    k <-x$getinv()
    if(!is.null(k)) {
        message("getting cached data")
        return(k)
    }
    m <- x$get()
    k <- solve(m, ...)
    x$setinv(k)
    k
        ## Return a matrix that is the inverse of 'x'
}
