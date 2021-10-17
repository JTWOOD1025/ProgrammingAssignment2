## inv represents NULL objects; set sets the value of the metrics using the NULL function; getInverse sets the inverse; getInverse gets the inverse.

## The below parent functions sets and gets the values of an inverse and stores as a list.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
          x <<- y
          inv <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The below function computes the inverse of the metrics created.

## inv gets the inverse of x; if inv is not null then returns the message "getting cached data" and returns the inv and is solved/computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
      inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
  }
