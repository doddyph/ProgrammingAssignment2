makeCacheMatrix <- function(x = matrix()) {
    invmatrix <- NULL
    
    set <- function(y) {
        x <<- y
        invmatrix <<- NULL
    }
    
    get <- function() x
    setInv <- function(xInvmatrix) invmatrix <<- xInvmatrix
    getInv <- function() invmatrix
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

cacheSolve <- function(x, ...) {
    invmatrix <- x$getInv()
    
    if (!is.null(invmatrix)) {
        message("getting cached data")
        return(invmatrix)
    }
    
    data <- x$get()
    invmatrix <- solve(data, ...)
    x$setInv(invmatrix)
    invmatrix
}