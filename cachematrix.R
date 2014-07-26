## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # initialize the variable inv
  set <- function(y) { # define the function set: set the value of the matrix
    x <<- y # assign y in another environment to x in current environment
    inv <<- NULL # initialize the variable inv
  }
  get <- function() x # define the function get: get the value of the matrix
  setinv <- function(inverse) inv <<- inverse # define the function setinv: set the value of the inverse
  getinv <- function() inv # define function getinv: get the value of the inverse
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv() # assign x's inv to inv
  if (!is.null(inv)) { # if x's inv is not NULL, return cached inv
    message("getting cached data")
    return (inv)
  }
  #if x's inv is NULL,
  data <- x$get() # assign x to data
  inv <- solve(data) # calcurate the inverse of data
  x$setinv(inv) # assign inv to x's inv
  inv
}
