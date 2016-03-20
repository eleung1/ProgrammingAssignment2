## Author: Eric Leung
##
## The following functions provide the ability to cache 
## the inverse of a matrix.
## 

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse as NULL
  cachedInverse <- NULL
  
  ## Setter: Set new matrix and invalidate the cached inverse
  set <- function(y){
    x <<- y
    cachedInverse <<- NULL
  }
  
  ## Getter: Get the matrix
  get <- function() x
  
  ## Setter: Set cached inverse
  setInverse <- function(inv) cachedInverse <<- inv
  
  ## Getter: Get cached inverse
  getInverse <- function() cachedInverse
  
  ## list of operations
  list(set = set, get = get, 
       setInverse = setInverse, getInverse = getInverse)
}

## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix().  If the inverse has already been calculated
## and the matrix has not changed, return the cached inverse.  Otherwise,
## compute the inverse and store it in cache.
cacheSolve <- function(x, ...) {
  
  ## Get the cached inverse in the special matrix object
  inv <- x$getInverse()
  
  ## Check and see if the cachedInverse exist
  if(!is.null(inv)){
    ## Cached inverse exist! returning it as-is.
    message("getting cached inverse!")
    return(inv)
  } 
  else {
    ## Cached inverse does not exist.
    ## Compute inverse and put it in cache.
    
    ## Get the matrix
    data <- x$get()
    
    ## compute inverse
    inv <- solve(data)
    
    ## Cache inverse
    x$setInverse(inv)
    return(inv)
  }
}
