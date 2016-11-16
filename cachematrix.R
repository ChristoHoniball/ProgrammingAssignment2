## Inverse the cache of the matrix object it creates
## Consists of 4 functions
## get: The vector "a" stored in the main function gets returned
## set: Applies a change to the stored vector from the main function
## setinversed: Stores a value in input variable b
## getinversed: Returns the variable from SetInversed


makeCacheMatrix <- function(a = matrix()) {

  b <- NULL
  set <- function(c) {
    a <<- c
    b <<- NULL
  }
  get <- function() a
  setinversed <- function(solve) b <<- solve
  getinversed <- function() b
  list(set = set, get = get,
       setinversed = setinversed,
       getinversed = getinversed)
  
}


## cacheSolve returns the inverse of the matrix determined by the previous function makeCacheMatrix
## In the case of the inverse already calculated "retrieve the inverse from cache" happens
## else
## b is respnsible for calculating the inverse, and the x$setmean(b) will stores it


cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'a'
  b <- a$getinverse()
  if(!is.null(b)) {
    message("Fetching the cached data")
    return(b)
  }
  data <- a$get()
  b <- solve(data, ...)
  a$setinverse(b)
  b
}
