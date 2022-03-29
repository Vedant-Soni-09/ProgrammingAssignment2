setwd('C:/Users/Vedant0509/Documents/Coursera-R')
##Matrix inversion is usually a costly computation and there may be some
##benefit to caching the inverse of a matrix rather than compute it repeatedly.
##Below are a pair of functions that are used to create a special object that
##stores a matrix and caches its inverse.
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
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


##This function computes the inverse of the special "matrix" created by
##makeCacheMatrix above. If the inverse has already been calculated (and the
##matrix has not changed), then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
