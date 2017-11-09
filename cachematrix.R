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
    setinv <- function(inv) inv_x <<- inv
    getinv <- function() inv_x
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv_x <- x$getinv()
    ##if you got the cached inverse matrix, you just return it
    if(!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    ##else you get the original matrix, solve the inverse, cache it, and return it
    mt <- x$get()
    inv_x <- solve(mt,...)
    x$setinv(mt)
    inv_x
}
