## Creating cachable matrix and inverse matrix
# Function makeCacheMatrix creates a cachable matrix and its inverse
# Function cacheSolve computes the inverse matrix of a given matrix only
# when the inverse has not already been computed and been stored in memory

## Creates a cachable matrix and its inverse
# Creates a list with 4 functions to
# 1. set the elements of the matrix
# 2. get the elements of the matrix
# 3. set the elements of the inverse matrix
# 4. get the elements of the inverse matrix
#
# Variable "inv" contains the elements of the inverse matrix, once
# it is set. If a new matrix is set, the old inverse matrix is 
# discarded
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invmatrix) inv <<- invmatrix
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Compute inverse matrix of cachable matrix
# Test whether inverse matrix has already been computed.
# If so, extract inverse matrix directly from cache.
# If not, compute inverse matrix and store result
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    message("computing inverse matrix")
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
