## Put comments here that give an overall description of what your
## functions do

## This function creates a 'special' matrix that will
## associate a set of functions with it. These functions are activated
## when called specifically.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Funciton checks to see whether x (via getinverse) has a value
## already set by checking for a NULL value. If no NULL value
## the cached version already set is returned
## Otherwise, the solve function is called (provided that a matrix has
## been associated to the special matrix), the newly calculated value is
## stored in the special matrix, and the inverse returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
