## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This pair of functions caches the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get<-function() x
  set_inv <- function(solve) m_inv <<- solve
  get_inv <- function() m_inv
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m_inv <- x$get_inv()
  if(!is.null(m_inv)) {
    message("Getting cache data")
    return(m_inv)
  }
  m <- x$get()
  m_inv <- solve(m, ...)
  x$set_inv(m_inv)
  m_inv
}
