makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function(){
    x
  }
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  getInverse <- function(){
    inv
  }
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  matr <- x$getMatrix()
  inv <- solve(matr, ...)
  x$setInverse(inv)
  inv
}
