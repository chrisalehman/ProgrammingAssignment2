## This file contains two functions. The first builds a "special matrix" list exposing functions for caching an input 
## matrix and its inverse. The second function computes the inverse and caches the result. 

## Takes an input matrix and returns a list of getter and setter functions for the matrix and its computed inverse:
## 1. set(m): caches a matrix and clears any computed inverse from the cache
## 2. get(): fetches a matrix from the cache
## 3. setsolve(s): caches a matrix inverse
## 4. getsolve(): fetches a matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  # sets the new matrix and purges the inverse from the cache
  set <- function(m) {
    x <<- m
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(
    set = set, 
    get = get, 
    setsolve = setsolve, 
    getsolve = getsolve
  )
}

## Takes the "special matrix" produced by makeCacheMatrix() and caches the computed inverse. If the inverse has already 
## been computed (and the cached matrix has not changed via $set), then cacheSolve retrieves the cached inverse instead
## recomputing it.
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
