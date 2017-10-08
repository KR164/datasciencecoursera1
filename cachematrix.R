## This pair of functions cache the inverse of a matrix.
## The function makeCacheMatrix builds a set of functions and 
## returns the functions within a list to the parent environment.
## It creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setsolve <- function(solve) m<<-solve
  getsolve <- function() m
  list( set=set, get=get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## The function cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m<-x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  }

