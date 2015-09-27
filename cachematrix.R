## makeCacheMatrix is used to create a matrix for which 
## the inverse can be cached and retrieved as needed

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  ## check if matrix can be inverted
  if(det(x) == 0){
    warning("Matrix is not invertible!")
    return(NA)
  }
  
  #defining the setter and getter functions to work with cache
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cachesolve is called to calculate the inverse of the cached matrix.
## The inverse is calculated using solve if it hasn't already been yet and stored in the cache.
## Subsequent calls to this method retrieve the inverse
## from the cache using the getinverse method.

cachesolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## is the inverse in the cache?
  m <- x$getinverse()
  if(!is.null(m)) {
    ## we have a cache hit
    message("getting cached inverse")
    return(m)
  }
  # cache miss! fresh calculation required
  data <- x$get()
  m <- solve(data, ...)
  # cache the result
  x$setinverse(m)
  m
}
