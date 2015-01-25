## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by
##  makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.
## R Programming Assignment 2: Lexical Scoping--caching the inverse of a matrix
## Usage:
## 1. First create a sqaure inversible matrix (e.g. x <- matrix(c(1,2,3,6,7,3,2,3,1),nrow=3, ncol=3) )
## 2. Call makeCacheMatrix to set it and save the output in a list (e.g. l <- makeCacheMatrix(x) )
## 3. Check if you can get the matrix (e.g. l$get())
## 4. Now call cacheSolve as cacheSolve(l) for the first time (it should calculate using solve, can't get it from cache)
## 5. Call it gain, it should now get the inverse from cache!

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the special "matrix"
## the special "matrix" which created with the makeCacheMatrix function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}