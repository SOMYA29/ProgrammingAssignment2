## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##  1   set the value of the matrix
##  2   get the value of the matrix
##  3   set the value of its inverse
##  4   get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setfunc <- function(n)
  {
    x <<- n
    inv <<- NULL
  }
  getfunc <- function(){x}
  setinvfunc <- function(inverse)
  {
    inv <<- inverse
  }
  getinvfunc <- function()
  {
    inv
  }
  list(setfunc = setfunc,getfunc = getfunc,
       setinvfunc = setinvfunc,
       getinvfunc = getinvfunc)
}


##  The following function calculates the inverse of the special "matrix" created with the above function. 
##  It first checks to see if the inverse has already been calculated. 
##  If so, it gets the inverse from the cache and skips the computation. 
##  Otherwise, it calculates the inverse of the matrix and sets the value of the matrix in the cache via the "setinvfunc" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvfunc()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getfunc()
  inv <- solve(data,...)
  x$setinvfunc(inv)
  inv
}
