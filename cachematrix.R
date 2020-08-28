## Wrote two functions "makeCacheMatrix" and "cacheSolve" in order to cache the
##inverse of  a matrix

##the function "makeCacheMatrix" can cache its inverse for the input by creating
##the matrix object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}


## cacheSolve computes the inverse of the  matrix from the previous 
##makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}