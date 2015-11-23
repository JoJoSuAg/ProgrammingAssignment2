## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This is function creates a special matrix which allows to store the original matrix and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ma <- NULL ##In this line, the value of matrix is reset
  set <- function(y) { ##Function which sets the value of the matrix
    x <<- y 
    ma <<- NULL ##Resetting the matrix
  }
  get <- function() x ##Function which gets the value of the matrix
  setinve <- function(inve) ma <<- inve ##Function which sets the value of the inverse
  getinve <- function() ma ##Function which gets the value of the inverse
  list(set = set, get = get,
       setinve = setinve,
       getinve = getinve) ##List of the functions of makeCacheMatrix 
}


## Write a short comment describing this function
## This function calculates the inverse of this special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ma <- x$getinve()
  if(!is.null(ma)) { ## Message
    message("getting cached data")
    return(ma)
  }
  data <- x$get()
  ma <- solve(data) ##Calculations to obtain the inverse
  x$setinve(ma)
  ma
}
