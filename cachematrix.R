## The makeCacheMatrix and cacheSolve functions cache the inverse of a matrix
## function. makeCacheMaatrix introduces a special "matrix" and cacheSolve
## retrieves its inverse.

## The makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mtx <- NULL
  # Set value and clear cache
  set <- function (y) {
    x <<- y 
    mtx <<- NULL
  }
  # get the value of the matrix, set the inverse, get the inverse
  get <- function () x
  setInv <- function (inverse) mtx <<- inverse
  getInv <- function () mtx
  # return list with functions that get matrix, set inverse, get inverse
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The cacheSolve matrix computes the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated then cachSolve
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mtx <- x$getInv()
  ## If cache is not empty, return what is there
  if (!is.null(mtx)){
    message("Getting Cached Data")
    return(mtx)
  } 
  ## When cache is empty, run calculations to get the value, cache, return
  mtx.data <- x$get()
  mtx <- solve(mtx.data)
  x$setInv(mtx)
  mtx
}
