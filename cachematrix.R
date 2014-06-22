## Programming assignment 2 provides a simple example on how to cache objects.  This
## assignment contains two functions.  The first function is called 'makeCacheMatrix'
## and it is a constructor function that creates a matrix that can cache its inverse.

## The second function checks to see if the inverse had already been calculated and stored in 
## cache.  If it has been stored in cache, the function returns the cached value.  If cache was
## NULL, the function creates the inverse and calls the object's 'cacheInverse()' function to store it.


####################################################################
##  Function:  makeCacheMatrix
##  
##  Description:  Constructs an object containing an invertible matrix and functions to act on that matrix
##
##  Arguments:  invertible matrix x
##
##  Functions:  cacheInverse(x) - sets the cache value to the object passed in
##              getInverse() - gets the value of cached Inverse
##              get() - returns the original matrix
###################################################################

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  originalMatrix <- x
  cacheInverse <- function(x) {inverseMatrix<<- x}
  getInverse <- function() inverseMatrix
  get <- function() originalMatrix
  list(cacheInverse=cacheInverse, getInverse=getInverse, get=get)
}


####################################################################
##  Function:  cacheSolve
##  
##  Description:  Returns inverse of matrix from cache if available.  If not found in cache, it 
##                calculates the inverse, stores it in cache and returns the inverse matrix
##
##  Arguments:  list object as returned from makeCacheMatrix()
##
##
###################################################################

cacheSolve <- function(x, ...) {
  if(is.null(x$getInverse())) {
    print("Creating Inverse Matrix")
    x$cacheInverse(solve(x$get()))
  } else {
    print("Retrieving Cached Inverse Matrix")
  }
  x$getInverse()      ## Return a matrix that is the inverse of 'x'
}
