## Caching the Inverse of a Matrix
## "setMatrix" sets the value of the Matrix
## "getMatrix" gets the value of the Matrix
## "setInverse" sets the inverse of the Matrix
## "getInverse" gets the inverse of the Matrix

## "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(Inverse) inv <<- Inverse
  getInverse <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## "cacheSolve" function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$getMatrix()
  inv<- solve(matrix, ...)
  x$setInverse(inv)
  inv
}
