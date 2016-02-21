## These functions give the user the ability to cache the inverse of a matrix
## so when needed subsequently it can just be returned.

## Write a short comment describing this function
## This function acts as a class for the inverse of a matrix
## it has set for the invertible matrix 
## and get functions for both the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  matInverse <- NULL
  set <- function(y) {
    x <<- y
    matInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) matInverse <<- inv
  getInverse <- function() matInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
## This function is called when 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, diag(nrow(data)), ...)
  x$setInverse(inv)
  inv
}
