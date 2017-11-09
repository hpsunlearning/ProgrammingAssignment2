## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## inv_x is where I store the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv_x <<- solve
    getinv <- function() inv_x
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv_x <- x$getinv()
    if(!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    mt <- x$get()
    inv_x <- solve(mt,...)
    x$setinv(mt)
    inv_x
        ## Return a matrix that is the inverse of 'x'
}
