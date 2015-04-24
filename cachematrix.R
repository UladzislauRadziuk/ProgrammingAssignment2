## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix that can cache an inputted matrix and its inverse and 
## contains 4 functions:
## 1.-2. Setting and getting a value of a matrix;
## 3.-4. Setting and getting a value of an inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL ## sets the value to NULL as default 
  set = function(y) {
    x <<- y ## caches the inputted matrix to check whether it was changed
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function calculates an inverse of the matrix created above. It checks if the inverse 
## has already been calculated, if not - it calculates the inverse an sets its in the cache. 

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)){ 
    message("getting cached data")
    return(inv)
  } 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}
