## Coursera R Programming: Assignment 2
## Demonstrates lexical scoping.
## KAlaimo

## Make a special matrix which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(i) inv <<- i
     getinverse <- function() inv
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return the inverse of a special matrix created by makeCacheMatrix.
## Assumes the matrix is invertible.
## If the inverse has already been computed, the cached matrix is returned.
## Otherwise, the inverse is computed, cached and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i     
}
