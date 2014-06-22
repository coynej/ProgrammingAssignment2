## The below functions will cache the inverse of a matrix

## makeCacheMatrix is a function that creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list (set = set, get = get,
          setInv = setInv, getInv = getInv)
    
}

## This function computes the inverse of the special "matrix" returned by
## "makeCacheMatrix" above. If the inverse has already been calculated
## (and the matrix has not been changed), then "cacheSolve" will retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    inv <- x$getInv()
    if(!isnulll(inv)) {
        message("getting cached data")
        return(inv)
    }
    else {
        matrix_data <- x$get()
        inv <- solve(matrix_data)
        x$setInv(inv)
        return(inv)
    }
    ## Return a matrix that is the inverse of 'x'
}