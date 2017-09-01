## the idea is that my first function will create a matrix object 
## which can chache its inverse
## The second function will compute the inverse of makeCacheMatrix() if the
## inverse has not all ready been calculated. Otherwise it will retieve the 
## invers from the chace.

## @z: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(z = matrix()) {
  
  inv = NULL
  set = function(y) {
    z <<- y
    inv <<- NULL
  }
  get = function() z
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## z is the output from makeCacheMatrix formula
## in the following I'll return the input to makeCacheMatrix() matrixs' inverse 
## note: if the inverse has already been calculated, it will take from cache 
## and will not calculate inverse

cacheSolve <- function(z, ...) {
  inv = z$getinv()
  
  
  if (!is.null(inv)){
    message("retrieving previously cached data")
    return(inv)
  }
  
  # oterwise inverse calculation occure here 
  mat.data = z$get()
  inv = solve(mat.data, ...)
  
  # using the simple setinv function i'm setting the inverse.
  z$setinv(inv)
  
  return(inv)
}
