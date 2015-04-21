## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix (x = matrix())
## As from the assignement:
## This function creates an "special" matrix object than can cache its inverse.
## It is assumed for this function that the matrix is always invertible.

makeCacheMatrix <- function(x = matrix()) {
      
      auxinverse <- NULL # Variable used to store the matrix inverse.
      
      # Set the value of the matrix
      set <- function (y) {
            x <<-y
            auxinverse <<- NULL
      }
      
      # Retrieves the value of the matrix
      get <- function() x
      
      # Set the inverse of the matrix
      setinv <- function (invmatrix) auxinverse <<- invmatrix
      
      # Get the inverse matrix.
      getinv <- function () auxinverse
      
      list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      invmatrix <<- x$getinv()
      if (is.null(invmatrix)) { # if there is no inverse matrix, compute it!
            maux <<- x$get()
            invmatrix <- solve(maux)
            x$setinv(invmatrix)
      } else {
            message("Cached data!")
      }
      
      invmatrix # returns the inversed matrix
}
