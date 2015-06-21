## This code contains a pair of functions that cache the inverse of the matrix
## README.md explains how the assignment is to be worked upon, the basic logic behind it
## and how it has to be completed on Git. 

##  makeCacheMatrix is a function which returns a list of functions
## This function creates a special "matrix" object that can cache its inverse.
## It contains the following functions:
## setMatrix: This is used to set the value of a matrix
#  getMatrix: This is used to get the value of a matrix
#  cacheInverse: This is used to the cache the inverse of a matrix
#  getInverse: This is used to get the cached value



makeCacheMatrix <- function(x = matrix()) {
  # m will be used to hold the cached value (or NULL in case nothing is cached)
  # Initially nothing is cached so m is set to NULL
  m <- NULL
  
  # setMatrix function is used to store the matrix
  setMatrix <- function(y) {
    x <<- y
    # "m <<- NULL" restores to null the value of the mean m, because the old mean of the old vector is not needed anymore.
    m <<- NULL
  }
  
  # getMatrix returns the matrix
  getMatrix <- function() x
  
  # The given argument is cached
  cacheInverse <- function(solve) {
    m <<- solve
  }
  
  # getInverse is used to obtain the cached value
  getInverse <- function() m
  
  
  # The following line of code is used to obtain a list so that when we assign makeCacheMatrix to an object, the object has all the 4 functions.
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # The following line of code returns the the cached value
  m <- x$getInverse()
  
  # If a cache value exists, it is returned
  if(!is.null(m) {
    message("Getting cached data")
    return(m)
  }
  
  # Otherwise, it calculates the inverse and stores it in the cache via catchInverse function.
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$cacheInverse(m)
  
  # The inverse is returned
  m
}
