## These functions create a type of matrix that caches the inverse to save time

## The next function creates a type of matrix that can cache a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list (set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## This function recovers the inverse from the cache if possible
## If the inverse has not been calculated, then this function does it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    mat1 <- x$get()
    inv <-solve(mat1,...)
    x$setinverse(inv)
    inv
}
