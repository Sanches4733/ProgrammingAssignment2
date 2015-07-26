## It is a pair of functions, that caches the inverse to a matrix
## It helps not to waste time on recomputing it

## This function makes a special list of functions<
## that caches the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)

}


## This function checks if the inverse matrix is
## cached, and computes it if it is not

cacheSolve <- function(x, ...) {
      s <- x$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
        ## Return a matrix that is the inverse of 'x'
}
