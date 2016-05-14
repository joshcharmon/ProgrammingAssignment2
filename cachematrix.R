## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix takes a matrix X and stores it in memory
## cacheSolve takes a matrix stored in memory, computes, and then displays the inverse.

## Write a short comment describing this function
## makeCacheMatrix uses scoping on the matrix and then stores the result in memory.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(cacheSolve) m <<- cacheSolve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setinv(m)
        m
}
