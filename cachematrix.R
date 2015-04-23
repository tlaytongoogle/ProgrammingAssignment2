## Implements a wrapper object that contains a single matrix. This wrapper,
## in addition to providing get and set access to its matrix,
## uses a built-in cache which stores the matrix's inverse,
## to avoid unnecessary recalculation of that value.
## Note that this implementation assumes only the first argument of cacheSolve()
## is used; if others are, the cache may provide a value that is not the
## correct result. I think that this is behavior is quite silly,
## but it mirrors the behavior of cachemean() the example.

## Wraps the provided matrix with a delegator that caches the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Solves for a wrapped matrix's inverse, using the wrapper's cached value if able.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
