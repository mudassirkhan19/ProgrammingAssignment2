## Function for creating special matrix that returns a list of functions
## for getting and setting matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setm <- function(y){
        x <<- y
        inv <<- NULL
    }
    getm <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list (
        setm = setm,
        getm = getm,
        setinv = setinv,
        getinv = getinv
    )
}


## Function for getting inverse from cache or solving it if not present in cache

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$getm()
    inv<-solve(data,...)
    x$setinv(inv)
    inv
}
