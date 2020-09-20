# Caching works by storing and intermediate result. 
# If the cache is empty, we want to store our result in the cache. 
# If the cache is filled and we need to calculate the inverse of 
# the same matrix again, we take our result directly from the 
# cache. 
# If we change the matrix, we flush the cache.

makeCacheMatrix <- function(x=matrix()) {
# We define a function which returns a list of four functions:
#   1) set sets the matrix
#   2) get retrieves the matrix
#   3) setinverse fills the cache with the inverted matrix
#   4) getinverse retrieves the inverted matrix from our cache
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_matrix) m <<- inverse_matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
# We first check if x already has an inverse in its cache.
# If so, we return the cache. If not, we calculate the inverse
# and set the cache.

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}