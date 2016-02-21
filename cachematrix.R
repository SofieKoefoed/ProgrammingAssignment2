## These functions should do what is needed to be done in the assignment 
## Caching the Inverse of a Matrix.

## The point of this function is to create a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(get = get,
       setsolve = setsolve,
       getsolve = getsolve
  )
}

## This function should compute the inverse of the matrix returned by the funktion
## makeCacheMatrix above. If the inverse has been calculated before and there is no 
## change in the matrix, then the cachesolve funktion should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  blob <- x$get()
  s <- solve(blob, ...)
  x$setsolve(s)
  s
}
