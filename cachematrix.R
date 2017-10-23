## Call the function makeCacheMatrix by storing the result
## in the variable to use it later with cacheSolve
## Example:
## myMatrix <- makeCacheMatrix(matrix(c(2, 3, -2, 1), 2))
## cacheSolve(myMatrix)


## makeCacheMatrix creates an object in cache that has a list
## of 4 functions that later can be called by cacheSolve
## 'inv' stores in cache the inverted x

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve asks for a on object created by makeCacheMatrix
## as its argument x. It checks its cache to see if there is already
## an inverted matrix. If 'inv' has a value, it is returned.
## If 'inv' is null, then solve the matrix, store the inverted value
## in cache and return it.


cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    if(!is.null(inv)) {
        message("getting the result from cache")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv
    return(inv)
}