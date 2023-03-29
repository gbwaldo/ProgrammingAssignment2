## Functions to cache inverse of matrix and retrieve its 
##inverse from the cache

## Creation of makeCache matrix object

makeCacheMatrix <- function(m = matrix()) {
    g <- NULL
    set <- function(y) {
            m <<- matrix
            g <<- NULL
    }
    get <- function() m
    setInverse <- function(inverse) g <<- inverse
    getInverse <- function() g
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computing the inverse of the matric returned by makeCachematric

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      ver <- x$getInverse()
      if(!is.null(ver)) {
          message("getting cached data")
          return(ver)
      }
      data <- x$get()
      ver <- solve(data)
      x$setInverse(ver)
      ver
}
