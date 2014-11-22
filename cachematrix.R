## Assignment # 2: Caching the Inverse of a Matrix

## A pair of functions that cache the inverse of a matrix.

## Function 1:
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # setting x equal to an empty matrix
    Inverse <- NULL
    # setting Inverse equal to NULL 
    set <- function(y) {
        x <<- y
        # set function assingment
        Inverse <<- NULL
        # inverse is re-set to NULL
    }
    get <- function() x
    # get function returns the matrix 
    setInverse <- function(solve) Inverse <<- solve
    # setInverse function assigns the argument to Inverse 
    getInverse <- function() Inverse
    # getInverse function returns Inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    # creates the list of functions
}

## Function # 2:
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    Inverse <- x$getInverse()
    # gets the most recent value of Inverse
    if(!is.null(Inverse)) {
        # if the value of Inverse is not NULL ...
        message("getting cached data")
        return(Inverse)
        # ... this will return that value
    }
    data <- x$get()
    Inverse <- solve(data, ...)
    # calculating the inverse and assigning it to Inverse 
    x$setInverse(Inverse)
    # setting Inverse to newly calculated value
    Inverse
}
