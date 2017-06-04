## Function makeCacheMatrix is used for initializing a matrix 
## input as argument "x = matrix()" It has four functions inside, which are 
## set(),get(), setinverse(), getinverse(). The first two are used to set or get input matrix
## whereas the last two are used to cache the inverse matrix of input matrix and get the 
## cached inverse matrix

## Function cacheSolve is used to calculate inverse matrix and stored the inversed matrix
## through x$setinverse(). Of course, it will check whether the inveserd matrix exist
## If exist, it will simply return the cached one and print "Getting cached inversed Matrix"

## Function makeCacheMatrix takes a matrix which is going to be inversed as an argument.
## It has for functions (set(), get(), setinverse(), getinverse()). set() and get() are used for 
## manipulate the original matrix. 
## getinverse() is used to get cached inversed matrix.
## setinverse() is used in function cacheSolve() to store the inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  set <- function(y) {
    x <<- y
    ix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) ix <<- solve
  getinverse <- function() ix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheSolve(x,...) is used to calculate the inverse matrix and call x$setinverse() function 
## to cache the inversed matrix. It checks whether there is a cached one first 
## by calling function x$getinverse(). If not, it will call solve() function to calculate inverse matrix
## and cache it by calling x$setinverse(), otherwise, it will return the cached matrix and print
## "Getting cached inversed Matrix"


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ix <- x$getinverse()
  if(!is.null(ix)) {
    message("Getting cached inversed Matrix")
    return(ix)
  }
  data <- x$get()
  ix <- solve(data, ...)
  x$setinverse(ix)
  ix
}
