## Put comments here that give an overall description of what your
## functions do

## cache de matrix on a variable

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) invMatrix <<- inverse
    getInv <- function() invMatrix
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## return the already calculated matrix or recalculate it
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
        
    # was it solved already?
    if( !is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # not calculated
    data<-x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}
