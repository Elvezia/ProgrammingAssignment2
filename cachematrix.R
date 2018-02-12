## makeCacheMatrix caches the inverse of a matrix
##cacheSolve either computes the inverse of the matrix returned by the previous funtion or retrieves the result from the cache, if the inverse has already been calculated

## The function overall calculates the inverse of a matrix and caches it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}

## Getting the result from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}