## These functions allow for the creation of a matrix object that can cache the mean
## and inverse of the matrix. This will only work for square matricies.

## This function creates a matrix holding object with the functions:
## 1 - set - set the matrix data
## 2 - get - return the matrix data
## 3 - setmean - calculate and store the mean
## 4 - getmean - return the stored mean value
## 5 - setinverse - calculate and store the inverse of the matrix
## 6 - getinverse - return the stored inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  s <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    s <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will check to see if the inverse of a makeCacheMatrix matrix object
## has been calculated. If not it will calculate it and return the value. If it has
## the function will print 'getting cached data' and return the cached value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}