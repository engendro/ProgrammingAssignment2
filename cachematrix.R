## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## 

## This function creates a list with 4 more functions, each one with the following functionality:
## - Get the value of the inverese of the matrix.
## - Set the value of the inverse of the matrix.
## - Set the value of the matrix.
## - Get the value of the matrix.
## This is similar to defining a class with 4 methods, as described above.
## The return value is a list with the functions commented above
## this function acts as a cache to stored the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function(){
    return(x)
  } 
  
  setInverse <- function(inverse){
    m <<- inverse
  } 
  
  getInverse <- function() {
    return(m)
  }  
  
  return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}


## This function returns the invers of the matrix. 
## First checks if the inverse has been already calculated and stored.
## If the inverse has been stored then returns the result and exit.
## If not has a cached inverse of the matrix, then calculates the inverse and then return the result, 
## and sets the result in the cache.
## This funcion, only can solve the inverse of a square matrix, so its assumed that the matrix, 
## will be always invertible.
cacheSolve <- function(x, ...) {
  m <- x$getInverse() # if an inverse has already been calculated this gets it
  
  if(!is.null(m)){ # check to see if cacheSolve has been run before
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setInverse(m)
  
  return(m)

}
